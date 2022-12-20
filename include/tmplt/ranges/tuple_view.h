#ifndef TMPLT_RANGES_TUPLE_VIEW_H
#define TMPLT_RANGES_TUPLE_VIEW_H

#include <ranges>
#include <tuple>
#include <concepts>
#include <cstddef>
#include <utility>
#include <variant>
#include <array>
#include <iterator>
#include <memory>

namespace tmplt::ranges
{

namespace detail
{

template<typename T, std::size_t I>
concept get_callable = requires(T& t)
{
    typename std::tuple_element<I, T>::type;
    { std::get<I>(t) } -> std::same_as<std::add_lvalue_reference_t<std::tuple_element_t<I, T>>>;
};

template<typename T, std::size_t ... I>
[[nodiscard]] consteval bool all_gets_callable(std::index_sequence<I...>) noexcept
{
    return (get_callable<T, I> && ...);
}

template<typename T, typename U>
concept not_same = !std::same_as<std::remove_cvref_t<T>, std::remove_cvref_t<U>>;

}

template<typename T>
concept tuple = requires(T& t)
{
    { std::tuple_size<T>::value } -> std::convertible_to<std::size_t>;
    requires detail::all_gets_callable<T>(std::make_index_sequence<std::tuple_size_v<T>>{});
};

template<tuple Tuple>
class tuple_view : public std::ranges::view_interface<tuple_view<Tuple>>
{
    static void construction(Tuple&);

    static void construction(Tuple&&) = delete;

    template<std::size_t I>
    class getter
    {
    public:
        explicit constexpr getter(Tuple& t) noexcept : t{std::addressof(t)}
        {}

        [[nodiscard]] constexpr decltype(auto) operator()() const noexcept
        {
            return std::get<I>(*t);
        }

    private:
        Tuple* t;
    };

    template<std::size_t I, typename Variant>
    [[nodiscard]] static constexpr auto variant_factory(Tuple& t) noexcept
    {
        return Variant{std::in_place_index<I>, t};
    }

    template<std::size_t ... I>
    [[nodiscard]] static consteval auto make_variant_factories(std::index_sequence<I...>) noexcept
    {
        return std::array{&variant_factory<I, std::variant<getter<I>...>>...};
    }

    static constexpr auto variant_factories = make_variant_factories(std::make_index_sequence<std::tuple_size_v<Tuple>>{});

    class sentinel
    {
    public:
        explicit constexpr sentinel() = default;
    };

    class iterator
    {
    public:
        using iterator_concept = std::random_access_iterator_tag;
        using value_type = std::invoke_result_t<std::ranges::range_value_t<decltype(variant_factories)>, Tuple&>;
        using difference_type = std::ptrdiff_t;

        explicit constexpr iterator() = default;

        explicit constexpr iterator(Tuple& t) noexcept : t{std::addressof(t)}
        {}

        [[nodiscard]] constexpr value_type operator*() const noexcept
        {
            return variant_factories[index](*t);
        }

        [[nodiscard]] constexpr value_type operator[](difference_type diff) const noexcept
        {
            return *(*this + diff);
        }

        constexpr iterator& operator++() noexcept
        {
            ++index;

            return *this;
        }

        constexpr iterator operator++(int) noexcept
        {
            auto tmp = *this;
            ++*this;

            return tmp;
        }

        constexpr iterator& operator--() noexcept
        {
            --index;

            return *this;
        }

        constexpr iterator operator--(int) noexcept
        {
            auto tmp = *this;
            --*this;

            return tmp;
        }

        constexpr iterator& operator+=(difference_type diff) noexcept
        {
            index += std::size_t(diff);

            return *this;
        }

        constexpr iterator& operator-=(difference_type diff) noexcept
        {
            index -= std::size_t(diff);

            return *this;
        }

        [[nodiscard]] friend constexpr iterator operator+(iterator it, difference_type diff) noexcept
        {
            return it += diff;
        }

        [[nodiscard]] friend constexpr iterator operator+(difference_type diff, iterator it) noexcept
        {
            return it + diff;
        }

        [[nodiscard]] friend constexpr iterator operator-(iterator it, difference_type diff) noexcept
        {
            return it -= diff;
        }

        [[nodiscard]] friend constexpr difference_type operator-(iterator const& lhs, iterator const& rhs) noexcept
        {
            return difference_type(lhs.index - rhs.index);
        }

        [[nodiscard]] friend constexpr difference_type operator-(iterator const& it, sentinel const&) noexcept
        {
            return difference_type(it.index - std::tuple_size_v<Tuple>);
        }

        [[nodiscard]] friend constexpr difference_type operator-(sentinel const&, iterator const& it) noexcept
        {
            return difference_type(std::tuple_size_v<Tuple> - it.index);
        }

        [[nodiscard]] friend constexpr bool operator==(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr auto operator<=>(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr bool operator==(iterator const& it, sentinel const&) noexcept
        {
            return it.index == std::tuple_size_v<Tuple>;
        }

    private:
        Tuple* t{nullptr};
        std::size_t index{};
    };

public:
    template<detail::not_same<tuple_view> T>
    requires std::convertible_to<T, Tuple&> && requires { construction(std::declval<T>()); }
    explicit constexpr tuple_view(T&& t) noexcept(noexcept(static_cast<Tuple&>(std::declval<T>()))) : t{std::addressof(static_cast<Tuple&>(std::forward<T>(t)))}
    {}

    constexpr auto begin() const noexcept
    {
        return iterator{*t};
    }

    constexpr auto end() const noexcept
    {
        return sentinel{};
    }

private:
    Tuple* t;
};

template<typename Tuple>
tuple_view(Tuple&) -> tuple_view<Tuple>;

}

namespace tmplt::views
{

struct tuple_view_adaptor
{
    template<typename T>
    [[nodiscard]] constexpr auto operator()(T&& t) const noexcept
    {
        return ::tmplt::ranges::tuple_view{std::forward<T>(t)};
    }
};

inline constexpr tuple_view_adaptor tuple{};

}

template<typename Tuple>
inline constexpr bool ::std::ranges::enable_borrowed_range<tmplt::ranges::tuple_view<Tuple>> = true;

#endif /* TMPLT_RANGES_TUPLE_VIEW_H */
