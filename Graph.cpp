//
// Created by XBY on 2022/6/28.
//
#include "static_string.h"
#include <cstdio>
#include <thread>

namespace TL {
template <typename T> struct Return { using type = T; };

struct Nil {};

template <typename... Ts> struct TypeList {
    using type = TypeList<Ts...>;
    static constexpr size_t size = sizeof...(Ts);

    template <typename... T> using append = TypeList<Ts..., T...>;

    template <template <typename...> typename T> using exportTo = T<Ts...>;
};

template <typename T> constexpr static bool IsTypeList_v = false;
template <typename... T>
constexpr static bool IsTypeList_v<TypeList<T...>> = true;
template <typename T>
concept TypeList_c = IsTypeList_v<T>;

template <typename IN, template <typename> class F> struct Map;
template <typename IN, template <typename> class F>
using Map_t = typename Map<IN, F>::type;
template <template <typename> class F, typename... Ts>
struct Map<TypeList<Ts...>, F> : TypeList<typename F<Ts>::type...> {};

template <typename IN, typename INIT, template <typename, typename> class OP>
struct FoldL : Return<INIT> {};
template <typename IN, typename INIT,
          template <typename, typename>
          class OP> // OP = CrossProduct::OuterAppend
using FoldL_t = typename FoldL<IN, INIT, OP>::type;
template <typename ACC, template <typename, typename> class OP, typename H,
          typename... Ts>
struct FoldL<TypeList<H, Ts...>, ACC, OP>
    : FoldL<TypeList<Ts...>, typename OP<ACC, H>::type, OP> {};

template <typename IN, template <typename> class F>
struct FindBy : Return<Nil> {};
template <typename IN, template <typename> class F>
using FindBy_t = typename FindBy<IN, F>::type;
template <typename H, typename... Ts, template <typename> class F>
struct FindBy<TypeList<H, Ts...>, F>
    : std::conditional_t<F<H>::value, Return<H>, FindBy<TypeList<Ts...>, F>> {};

template <typename... IN> struct Concat;
template <typename... IN> using Concat_t = typename Concat<IN...>::type;
template <> struct Concat<> : TypeList<> {};
template <typename IN> struct Concat<IN> : IN {};
template <typename IN, typename IN2>
struct Concat<IN, IN2> : IN2::template exportTo<IN::template append> {};
template <typename IN, typename IN2, typename... Rest>
struct Concat<IN, IN2, Rest...> : Concat_t<Concat_t<IN, IN2>, Rest...> {};

template <typename IN, template <typename> typename P, typename S = TypeList<>,
          typename R = TypeList<>>
struct Partition {
    struct type {
        using satisfied = S;
        using rest = R;
    };
};
template <typename IN, template <typename> class P>
using Partition_t = typename Partition<IN, P>::type;
template <typename H, typename... Ts, template <typename> typename P,
          typename S, typename R>
struct Partition<TypeList<H, Ts...>, P, S, R>
    : std::conditional_t<
          P<H>::value,
          Partition<TypeList<Ts...>, P, typename S::template append<H>, R>,
          Partition<TypeList<Ts...>, P, S, typename R::template append<H>>> {};

template <typename IN, template <typename, typename> class CMP>
struct Sort : TypeList<> {};
template <typename IN, template <typename, typename> class CMP>
using Sort_t = typename Sort<IN, CMP>::type;

template <template <typename, typename> class CMP, typename H, typename... Ts>
class Sort<TypeList<H, Ts...>, CMP> {
    template <typename E> using LT = CMP<E, H>;
    using P = Partition_t<TypeList<Ts...>, LT>;
    using SmallerSorted =
        Sort_t<typename P::satisfied, CMP>; // Ts...中的后继含有H的
    using BiggerSorted = Sort_t<typename P::rest, CMP>; // Ts...中的后继不含H的

public:
    using type =
        Concat_t<typename SmallerSorted::template append<H>, BiggerSorted>;
};

template <typename IN, template <typename> class P>
using Filter_t = typename Partition_t<IN, P>::satisfied;

template <typename IN> class Flatten {
    struct impl {
        template <typename ACC, typename E>
        struct Append : ACC::template append<E> {};

        template <typename ACC, typename... Ts>
        struct Append<ACC, TypeList<Ts...>>
            : Concat_t<ACC, typename Flatten<TypeList<Ts...>>::type> {};
    };
    template <typename ACC, typename E>
    using Append = typename impl::template Append<ACC, E>;

public:
    using type = FoldL_t<IN, TypeList<>, Append>;
};

template <typename IN> using Flatten_t = typename Flatten<IN>::type;

template <typename IN, typename E> struct Elem : std::false_type {};
template <typename IN, typename E> constexpr bool Elem_v = Elem<IN, E>::value;
template <typename E, typename... Ts>
struct Elem<TypeList<Ts...>, E>
    : std::bool_constant<(std::is_same_v<E, Ts> || ...)> {};

template <typename IN> class Unique {
    template <typename ACC, typename E>
    struct Append : std::conditional_t<Elem_v<ACC, E>, ACC,
                                       typename ACC::template append<E>> {};

public:
    using type = FoldL_t<IN, TypeList<>, Append>;
};

template <typename IN> using Unique_t = typename Unique<IN>::type;

//
template <typename F, typename T> struct Connection {
    using FROMs = F;
    using TOs = T;
};

template <typename T, typename OUT = TypeList<>> struct Chain {};
template <typename F, typename T, typename OUT>
struct Chain<auto (*)(F)->T, OUT> {
private:
    using To = typename Chain<T, OUT>::From;

public:
    using From = F;
    using type = typename Chain<
        T, typename OUT::template append<Connection<From, To>>>::type;
};
template <typename F, typename OUT> struct Chain<auto (*)(F)->void, OUT> {
    using From = F;
    using type = OUT;
};

template <typename F, typename T> struct OneToOneLink {
    using FROM = F;
    using TO = T;
    /*constexpr void build() {
        std::get<JobCb<FROM>>(jobsCb).job_.precede(
            std::get<JobCb<TO>>(jobsCb).job_);
        printf("");
    };*/
};

// 输入<FromTasks, ToTasks>, 输出OneToOneLinkSet
template <typename FROMs, typename TOs, typename = void>
struct BuildOneToOneLink;
template <typename... Fs, typename Ts>
struct BuildOneToOneLink<TypeList<Fs...>, Ts> {
    using type = Concat_t<typename BuildOneToOneLink<Fs, Ts>::type...>;
};
template <typename F, typename... Ts>
struct BuildOneToOneLink<F, TypeList<Ts...>,
                         std::enable_if_t<!IsTypeList_v<F>>> {
    using type = TypeList<OneToOneLink<F, Ts>...>;
};
} // namespace TL
using namespace TL;
namespace details {
template <typename... Links> class TaskAnalyzer {
    template <typename Link> class OneToOneLinkSetF {
        using FromTaskList = typename Link::FROMs;
        using ToTaskList = typename Link::TOs;

    public:
        using type = typename BuildOneToOneLink<FromTaskList, ToTaskList>::type;
    };

public:
    using AllTasks =
        Unique_t<Concat_t<typename Links::FROMs..., typename Links::TOs...>>;
    using OneToOneLinkSet =
        Unique_t<Flatten_t<Map_t<TypeList<Links...>, OneToOneLinkSetF>>>;
};
template <typename DEP> struct DesAncOutPut {
    struct type {
        using Task = typename DEP::Task;
        using DesAnc = typename DEP::AllDesAncs;
    };
};
template <typename Analyzer> class DesAncMap { // class
    template <typename One2One>
    struct GetFrom : Return<typename One2One::FROM> {};
    template <typename One2One> struct GetTo : Return<typename One2One::TO> {};

    template <template <typename> typename OP1,
              template <typename> typename OP2,
              typename TaskSet = typename Analyzer::OneToOneLinkSet>
    struct Find_Direct_DesAnc {
        template <typename T> class Find {
            template <typename C>
            struct Dependency : std::is_same<typename OP1<C>::type, T> {};
            using TaskDesAncs = Filter_t<TaskSet, Dependency>;

        public:
            struct type {
                using Task = T;
                using DesAnc = Map_t<TaskDesAncs, OP2>;
            };
        };
    };
    // [Direct_AncDesMap_t<Task, Descendants>, ...]
    using Direct_DesMap_t = typename Map_t<
        typename Analyzer::AllTasks,
        Find_Direct_DesAnc<GetFrom, GetTo>::template Find>::type; //直接后继

    using Direct_AncMap_t = typename Map_t<
        typename Analyzer::AllTasks,
        Find_Direct_DesAnc<GetTo, GetFrom>::template Find>::type; //直接后继

    template <typename T, typename DECENDS, typename Direct_Map_t>
    class FindAllDesAns {
        template <typename ACC, typename Job>
        struct AppendDes { // JOB = DECENDS[n]
            template <typename DEP> struct JDepsCond {
                constexpr static bool value =
                    std::is_same_v<typename DEP::Task, Job> &&
                    !std::is_same_v<T, Job>; //环形检测
            };                               // DEP(T) == JOB
            using DepsResult =
                FindBy_t<Direct_Map_t,
                         JDepsCond>; // 从邻接表查找Task的后继节点列表

            using type = Concat_t<
                typename ACC::template append<Job>,
                typename FindAllDesAns<T, DepsResult, Direct_Map_t>::type>;
        };

    public:
        using type = FoldL_t<typename DECENDS::DesAnc, TypeList<>,
                             AppendDes>; // DECENDS =
        // Direct_AncDesMap_t[n]::Descendants
    };

    template <typename T, typename Direct_Map_t>
    class FindAllDesAns<T, Nil, Direct_Map_t> {
    public:
        using type = TypeList<>;
    };
    template <typename Direct_Map_t> struct FindTaskAllDesAncs {
        template <typename DEP> struct Find {
            struct type {
                using Task = typename DEP::Task;
                using AllDesAncs =
                    typename FindAllDesAns<Task, DEP,
                                           Direct_Map_t>::type; // TypeList
            };
        };
    };

public:
    struct type {
        using Des = Map_t<Direct_DesMap_t,
                          FindTaskAllDesAncs<Direct_DesMap_t>::template Find>;
        using Anc = Map_t<Direct_AncMap_t,
                          FindTaskAllDesAncs<Direct_AncMap_t>::template Find>;
    };
};

template <typename DesMap, template <typename> typename Getter>
class TopolSort { // class
    template <typename LHS, typename RHS> struct CMP {
        static constexpr bool value =
            LHS::AllDesAncs::size <
            RHS::AllDesAncs::size; // value为true则LHS前移
    };

public:
    using type = Map_t<Sort_t<DesMap, CMP>,
                       Getter>; // FindJobAllDescendants<Task,
    // AllDescendants>[n]
};
} // namespace details

enum class TaskflowType { Dynamic, Static };
enum class RunType { Parallel, Async, Topology };

template <auto... Func> struct Task {
    constexpr auto operator()() { return (Func(), ...); }
}; // mutiply tasks
template <static_string name, auto Func> struct Task<name, Func> {
    constexpr auto operator()() { return Func(); }
}; // single task
template <int N, Task... Tasks> struct Task<N, Tasks...> {
    constexpr auto operator()() { (std::thread(Tasks).join(), ...); }
}; // parell tasks

template <typename... Chains> struct Taskflow { // static
    template <typename... Tasks> struct Runable;
    template <static_string... names, auto... J>
    struct Runable<Task<names, J>...> {
        static constexpr void Run() { (J(), ...); }
    };
    using Connections =
        Unique_t<Flatten_t<TypeList<typename Chain<Chains>::type...>>>;
    using Analyzer =
        typename Connections::template exportTo<details::TaskAnalyzer>;

    class TopolFlow {
        template <typename DEP> struct GetTask : Return<typename DEP::Task> {};

    public:
        using type = typename details::TopolSort<
            typename details::DesAncMap<Analyzer>::type::Anc,
            GetTask>::type::template exportTo<Runable>;
    };
    class ParelFlow {
        template <typename DEP> struct GetTaskDesAnc : Return<DEP> {};
        using OriginTaskAncLists =
            Map_t<typename details::TopolSort<
                      typename details::DesAncMap<Analyzer>::type::Anc,
                      GetTaskDesAnc>::type,
                  details::DesAncOutPut>;
        using OriginTaskDesLists =
            Map_t<typename details::TopolSort<
                      typename details::DesAncMap<Analyzer>::type::Des,
                      GetTaskDesAnc>::type,
                  details::DesAncOutPut>;

    }; // TODO

public:
    template <RunType runType = RunType::Topology> static constexpr auto Run() {
        if constexpr (runType == RunType::Topology)
            TopolFlow::type::Run();
        if constexpr (runType == RunType::Parallel)
            ;
        return Self();
    }
    constexpr static TaskflowType type = TaskflowType::Static;
    using Self = Taskflow<Chains...>;
};

template <> class Taskflow<> { // dynamic
    constexpr static TaskflowType type = TaskflowType::Dynamic;
};

#define def_task(name, ...) using name = Task<#name, __VA_ARGS__>;
#define task(...) auto (*)(TypeList<__VA_ARGS__>)
#define $(...) __VA_ARGS__->void
#define def_simple(name)                                                       \
    using name = Task<static_string(#name),                                    \
                      []() -> void { printf("This is " #name "\n"); }>;

template <size_t Des_size> struct Result {
    Result() {
        for (size_t i = 0; i < Des_size; ++i)
            Des[i] = true;
    };
    bool Des[Des_size];
}; // TODO

auto main() -> int {
    def_simple(A) def_simple(B) def_simple(C) def_simple(D) def_simple(E)
        def_simple(S) def_simple(X) def_simple(Y) def_simple(Z) def_simple(T);

    Taskflow<$(task(S)->task(A, D, E)),
             $(task(D, A)->task(B)->task(X)->task(Y)->task(Z)->task(T)),
             $(task(D)->task(T)), $(task(D)->task(T)), $(task(E)->task(T)),
             $(task(B, E)->task(C)->task(X)->task(Y)->task(Z)->task(T))>::Run();
};
