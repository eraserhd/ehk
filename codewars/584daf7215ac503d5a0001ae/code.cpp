#include <cassert>
#include <cctype>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <memory>
#include <string>
#include <list>

// Greenspuns 10th rule?
struct Value
{
    int64_t _repr;
    enum Type { Pair, Number, Symbol, };
    inline Type type() const { return (Type)(_repr&3); }

    Value()                     : _repr(0)                  {}
    Value(int64_t number)       : _repr((number<<2)|Number) {}
    Value(char symbol)          : _repr((symbol<<2)|Symbol) {}
    Value(Value car, Value cdr) : _repr(alloc(car,cdr))     {}

    inline bool operator == (const Value& rhs) const
    {
        if (_repr == rhs._repr) return true;
        if (type() != Pair or rhs.type() != Pair) return false;
        if (_repr == 0 or rhs._repr == 0) return false;
        return car() == rhs.car() and cdr() == rhs.cdr();
    }

    inline bool operator != (const Value& rhs) const { return !operator == (rhs); } 

    inline Value car() const { return pair()->first; }
    inline Value cdr() const { return pair()->second; }

    static const Value NIL;
    static void clear_heap() { heap.clear(); }

private:
    static std::list<std::pair<Value,Value>> heap;
    static int64_t alloc(Value car, Value cdr)
    {
        heap.push_front(std::make_pair(car, cdr));
        return reinterpret_cast<int64_t>(&heap.front());
    }
    std::pair<Value,Value>* pair() const
    {
        return reinterpret_cast<std::pair<Value,Value>*>(_repr);
    }
};

template<typename Iterator>
int64_t parse_integer(Iterator& begin)
{
    int64_t result = 0;
    while (isdigit(*begin))
        result = 10*result + (*begin++ - '0');
    return result;
}

template<typename Iterator>
Value parse_rec(Iterator& begin)
{
    Value result;
    switch (*begin++)
    {
    case '-':
        if (isdigit(*begin))
            result = Value{-parse_integer(begin)};
        else
            result = Value{'-'};
        break;
    case '0' ... '9':
        --begin;
        result = Value{parse_integer(begin)};
        break;
    case '(':
        {
            Value backwards = Value::NIL;
            while (*begin != ')')
                backwards = Value{parse_rec(begin),backwards};
            ++begin;
            for (; backwards != Value::NIL; backwards = backwards.cdr())
                result = Value{backwards.car(), result};
        }
        break;
    default:
        result = Value{*(begin-1)};
        while (*begin and isalpha(*begin))
            ++begin;
        break;
    }
    while (*begin and isspace(*begin))
        ++begin;
    return result;
}

template<typename T>
Value parse(T const& what)
{
    auto it = std::cbegin(what);
    return parse_rec(it);
}

std::list<std::pair<Value,Value>> Value::heap;
const Value Value::NIL{};

template<typename Arg1, typename Arg2>
Value L(Arg1 arg1, Arg2 arg2)
{
    return Value{Value{arg1, Value{arg2, Value::NIL}}};
}
template<typename Arg1, typename Arg2, typename Arg3>
Value L(Arg1 arg1, Arg2 arg2, Arg3 arg3)
{
    return Value{Value{arg1, Value{arg2, Value{arg3, Value::NIL}}}};
}

Value differentiate(Value expr)
{
    if (expr == Value{'x'})           return {int64_t(1)};
    if (expr.type() == Value::Number) return {int64_t(0)};
    if (expr.car() == Value{'^'})
        return L('^', L('*', expr.cdr().car(), expr.car()), L('-', expr.cdr().car(), int64_t(1)));
    assert(false);
}

std::string diff(const std::string& s)
{
    parse(s);
    return "";
}

#ifdef TEST
int main(int argc, const char *argv[])
{
    assert(Value{int64_t(42)} == parse("42"));
    assert(Value{'s'} == parse("sin"));
    assert(Value{int64_t(-42)} == parse("-42"));
    assert(Value::NIL == parse("()"));
    assert(L('+', 'x', int64_t(0)) == parse("(+  x 0 )"));
}
#endif
