#ifndef TMPLT_RANGES_META_IOTA_VIEW_H
#define TMPLT_RANGES_META_IOTA_VIEW_H

#include <cstddef>
#include <ranges>
#include <variant>
#include <array>
#include <utility>
#include <type_traits>
#include <iterator>

namespace tmplt::ranges
{

template<std::size_t Count>
class meta_iota_view : public std::ranges::view_interface<meta_iota_view<Count>>
{
    template<std::size_t I, typename Variant>
    [[nodiscard]] static constexpr auto variant_factory() noexcept
    {
        return Variant{std::in_place_index<I>};
    }

    template<std::size_t ... I>
    [[nodiscard]] static consteval auto make_variant_factories(std::index_sequence<I...>) noexcept
    {
        return std::array{&variant_factory<I, std::variant<std::integral_constant<std::size_t, I>...>>...};
    }

    static constexpr auto variant_factories = make_variant_factories(std::make_index_sequence<Count>{});

    class sentinel
    {
    public:
        explicit constexpr sentinel() = default;
    };

    class iterator
    {
    public:
        using iterator_concept = std::random_access_iterator_tag;
        using value_type = std::invoke_result_t<std::ranges::range_value_t<decltype(variant_factories)>>;
        using difference_type = std::ptrdiff_t;

        explicit constexpr iterator() = default;

        [[nodiscard]] constexpr value_type operator*() const noexcept
        {
            return variant_factories[index]();
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
            return difference_type(it.index - Count);
        }

        [[nodiscard]] friend constexpr difference_type operator-(sentinel const&, iterator const& it) noexcept
        {
            return difference_type(Count - it.index);
        }

        [[nodiscard]] friend constexpr bool operator==(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr auto operator<=>(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr bool operator==(iterator const& it, sentinel const&) noexcept
        {
            return it.index == Count;
        }

    private:
        std::size_t index{};
    };

public:
    static constexpr auto begin() noexcept
    {
        return iterator{};
    }

    static constexpr auto end() noexcept
    {
        return sentinel{};
    }
};

}

namespace tmplt::views
{

template<std::size_t Count>
struct meta_iota_view_adaptor
{
    [[nodiscard]] constexpr auto operator()() const noexcept
    {
        return ::tmplt::ranges::meta_iota_view<Count>{};
    }
};

template<std::size_t Count>
inline constexpr meta_iota_view_adaptor<Count> meta_iota{};

}

template<std::size_t Count>
inline constexpr bool ::std::ranges::enable_borrowed_range<tmplt::ranges::meta_iota_view<Count>> = true;

#endif /* TMPLT_RANGES_META_IOTA_VIEW_H */
