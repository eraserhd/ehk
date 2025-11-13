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
end

defmodule Triangle do
  defstruct [:a, :b]

  # Per the shoelace formula
  def signed_area(%__MODULE__{a: %Point{x: ax, y: ay}, b: %Point{x: bx, y: by}}), do: (ay + by)*(ax - bx)/2.0

  def from_points(ps), do: Enum.zip(ps, tl(ps) ++ [hd(ps)]) |> Enum.map(fn {a, b} -> %Triangle{a: a, b: b} end)
end

if System.get_env("RUN_TESTS") do
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
end

