#pragma once

#include <type_traits>

#define ENUM_BINARY_OPERATE_ENABLE(Enum)                                       \
    static_assert(                                                             \
        std::is_enum_v<                                                        \
            Enum> && std::is_integral_v<std::underlying_type_t<Enum>>);        \
    template <typename T>                                                      \
    concept Enum##OperatorType =                                               \
        std::is_same_v<T, Enum> || std::is_integral_v<T>;                      \
    template <typename T, typename U>                                          \
    concept Enum##atLeastOneArg =                                              \
        std::is_same_v<T, Enum> || std::is_same_v<U, Enum>;                    \
    constexpr inline Enum &operator|=(Enum &x,                                 \
                                      Enum##OperatorType auto &&y) noexcept {  \
        return x = static_cast<Enum>(                                          \
                   static_cast<std::underlying_type_t<Enum>>(x)                \
                   | static_cast<std::underlying_type_t<Enum>>(y));            \
    }                                                                          \
    constexpr inline Enum &operator&=(Enum &x,                                 \
                                      Enum##OperatorType auto &&y) noexcept {  \
        return x = static_cast<Enum>(                                          \
                   static_cast<std::underlying_type_t<Enum>>(x)                \
                   & static_cast<std::underlying_type_t<Enum>>(y));            \
    }                                                                          \
    constexpr inline Enum &operator^=(Enum &x,                                 \
                                      Enum##OperatorType auto &&y) noexcept {  \
        return x = static_cast<Enum>(                                          \
                   static_cast<std::underlying_type_t<Enum>>(x)                \
                   ^ static_cast<std::underlying_type_t<Enum>>(y));            \
    }                                                                          \
    template <Enum##OperatorType T, Enum##OperatorType U>                      \
    requires Enum##atLeastOneArg<                                              \
        T, U> constexpr inline std::underlying_type_t<Enum>                    \
    operator|(T x, U y) noexcept {                                             \
        return static_cast<std::underlying_type_t<Enum>>(x)                    \
               | static_cast<std::underlying_type_t<Enum>>(y);                 \
    }                                                                          \
    template <Enum##OperatorType T, Enum##OperatorType U>                      \
    requires Enum##atLeastOneArg<                                              \
        T, U> constexpr inline std::underlying_type_t<Enum>                    \
    operator&(T x, U y) noexcept {                                             \
        return static_cast<std::underlying_type_t<Enum>>(x)                    \
               & static_cast<std::underlying_type_t<Enum>>(y);                 \
    }                                                                          \
    template <Enum##OperatorType T, Enum##OperatorType U>                      \
    requires Enum##atLeastOneArg<                                              \
        T, U> constexpr inline std::underlying_type_t<Enum>                    \
    operator^(T x, U y) noexcept {                                             \
        return static_cast<std::underlying_type_t<Enum>>(x)                    \
               ^ static_cast<std::underlying_type_t<Enum>>(y);                 \
    }                                                                          \
    constexpr inline bool operator!(Enum x) noexcept {                         \
        return !static_cast<std::underlying_type_t<Enum>>(x);                  \
    }                                                                          \
    constexpr inline Enum operator~(Enum x) noexcept {                         \
        return static_cast<Enum>(                                              \
            ~static_cast<std::underlying_type_t<Enum>>(x));                    \
    }                                                                          \
    template <Enum##OperatorType T, Enum##OperatorType U>                      \
    requires Enum##atLeastOneArg<T, U> constexpr inline bool operator>(        \
        T x, U y) noexcept {                                                   \
        return static_cast<std::underlying_type_t<Enum>>(x)                    \
               > static_cast<std::underlying_type_t<Enum>>(y);                 \
    }
