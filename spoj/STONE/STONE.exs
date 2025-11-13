defmodule Point do
  defstruct x: 0, y: 0

  def left_bottom_origin(ps), do: %Point{
    x: Enum.min(Enum.map(ps, fn %Point{x: x} -> x end)) - 100,
    y: Enum.min(Enum.map(ps, fn %Point{y: y} -> y end)) - 100,
  }

  def right_bottom_origin(ps), do: %Point{
    x: Enum.max(Enum.map(ps, fn %Point{x: x} -> x end)) + 100,
    y: Enum.min(Enum.map(ps, fn %Point{y: y} -> y end)) - 100,
  }

  def sub(%Point{x: ax, y: ay}, %Point{x: bx, y: by}), do: %Point{x: ax - bx, y: ay - by}

  def shift_origin_to(ps, origin), do: Enum.map(ps, fn p -> sub(p, origin) end)
end

defmodule Triangle do
  defstruct [:a, :b]

  # Per the shoelace formula
  def signed_area(%__MODULE__{a: %Point{x: ax, y: ay}, b: %Point{x: bx, y: by}}), do: (ay + by)*(ax - bx)/2.0

  def from_points(ps), do: Enum.zip(ps, tl(ps) ++ [hd(ps)]) |> Enum.map(fn {a, b} -> %Triangle{a: a, b: b} end)
end


defmodule Solution do

  def find_bisecting_angle(ts) do
    # FIXME:
    0.0
  end

  def intersection_point(%Point{x: ax, y: ay}, aslope, %Point{x: bx, y: by}, bslope) do
    # y = aslope*(x - ax) + ay
    # y = bslope*(x - bx) + by
    #
    # aslope*(x - ax) + ay = bslope*(x - bx) + by
    # aslope*x - aslope*ax + ay = bslope*x - bslope*bx + by
    # aslope*x - bslope*x = aslope*ax - ay - bslope*bx + by
    # x*(aslope - bslope) = aslope*ax - ay - bslope*bx + by
    # x = (aslope*ax - ay - bslope*bx + by) / (aslope - bslope)
    x = (aslope*ax - ay - bslope*bx + by) / (aslope - bslope)
    y = aslope*(x - ax) + ay
    %Point{x: x, y: y}
  end

  def solve(ps) do
    lbo = Point.left_bottom_origin(ps)
    lbo_ps = Point.shift_origin_to(ps, lbo)
    lbo_ts = Triangle.from_points(lbo_ps)
    lbo_angle = find_bisecting_angle(lbo_ts)

    rbo = Point.right_bottom_origin(ps)
    rbo_ps = Point.shift_origin_to(ps, rbo)
    rbo_ts = Triangle.from_points(rbo_ps)
    rbo_angle = find_bisecting_angle(rbo_ts)

    intersection_point(lbo, :math.tan(lbo_angle), rbo, :math.tan(rbo_angle))
  end

  def main() do
    test_cases = IO.read(:line) |> String.trim() |> String.to_integer()
    for _ <- 1..test_cases do
      points = IO.read(:line) |> String.trim() |> String.to_integer()
      ps = (1..points) |>
        Enum.map(fn _ ->
          [x, y] = IO.read(:line) |>
           String.trim() |>
           String.split(~r/\s+/) |>
           Enum.map(&String.to_integer/1)
          %Point{x: x, y: y}
        end)

      %Point{x: x, y: y} = solve(ps)
      IO.puts("#{x} #{y}")
    end
  end
end

case System.argv() do
  ["--test"] ->
    ExUnit.start()

    defmodule TriangleTest do
      use ExUnit.Case
      test "Triangles can compute signed area" do
        t1 = %Triangle{a: %Point{x: 0, y: 7}, b: %Point{x: 3, y: 0}}
        assert 7.0*3.0/2.0 == abs(Triangle.signed_area(t1))
        t2 = %Triangle{a: %Point{x: 3, y: 0}, b: %Point{x: 0, y: 7}}
        assert -Triangle.signed_area(t2) == Triangle.signed_area(t1)
      end

      test "Can make Triangles from Points" do
        ts = Triangle.from_points([
          %Point{x: -10, y: 10},
          %Point{x: 10, y: 10},
          %Point{x: 10, y: -10},
          %Point{x: -10, y: -10},
        ])
        assert [
          %Triangle{a: %Point{x: -10, y: 10}, b: %Point{x: 10, y: 10}},
          %Triangle{a: %Point{x: 10, y: 10}, b: %Point{x: 10, y: -10}},
          %Triangle{a: %Point{x: 10, y: -10}, b: %Point{x: -10, y: -10}},
          %Triangle{a: %Point{x: -10, y: -10}, b: %Point{x: -10, y: 10}},
        ] == ts
        assert 20*20 == abs(ts |> Enum.map(&Triangle.signed_area/1) |> Enum.sum())
      end
    end

    defmodule PointTest do
      use ExUnit.Case
      test "Can find origins from a set of points" do
        ps = [
          %Point{x: -10, y: 10},
          %Point{x: 10, y: 10},
          %Point{x: 10, y: -10},
          %Point{x: -10, y: -10},
        ]
        assert %Point{x: -110, y: -110} == Point.left_bottom_origin(ps)
        assert %Point{x: 110, y: -110} == Point.right_bottom_origin(ps)
      end
    end

    defmodule SolutionTest do
      use ExUnit.Case

      test "Can find the intersection of two point-slope lines" do
        p = Solution.intersection_point(%Point{x: 0, y: 0}, 0.5, %Point{x: 100, y: 0}, -0.5)
        assert %Point{x: 50.0, y: 25.0} == p
      end

    end

  _ ->
    Solution.main
end

