defmodule Point do
  def parse(p) do
    String.split(p, " ") |> Enum.map(fn coord -> Integer.parse(coord) |> elem(0) end) |> List.to_tuple
  end
end

defmodule Stone do
  def read_integer, do: IO.gets("") |> Integer.parse |> elem(0)

  def repeat(0, _), do: :ok
  def repeat(n, fun) do
    fun.()
    repeat(n-1, fun)
  end

  def adjust1(v1, v2, :min), do: min(v1,v2)
  def adjust1(v1, v2, :max), do: max(v1,v2)

  def extra_offset(v, :min), do: v-1
  def extra_offset(v, :max), do: v+1

  def offset([{x, y} | rest] = points, x_adjust, y_adjust) do
    {x_offset, y_offset} = rest |>
      Enum.reduce({x, y}, fn {x, y}, {accx, accy} ->
        {adjust1(accx, x, x_adjust), adjust1(accy, y, y_adjust)}
      end)
    x_offset = extra_offset(x_offset, x_adjust)
    y_offset = extra_offset(y_offset, y_adjust)

    adjusted_points = points |>
      Enum.map(fn {x, y} -> {x-x_offset, y-y_offset} end)

    {x_offset, y_offset, adjusted_points}
  end

  def bisect(points, x_adjust, y_adjust) do
    # The algorithm requires 0,0 to be outside of the polygon
    {x_offset, y_offset, adjusted_points} = offset(points, x_adjust, y_adjust)

    # Using the Shoelace formula, modifying the triangle interpretation:
    #
    # Make a list of { start_angle, area_per_radian }, so
    # that a shape that has [{ 0, 10 }, { 0.5, 0 }] would have an area
    # of 5, since there's 0.5 radians between the two angles and the shape
    # has an area of 10 per radian.
    #
    start_angle_to_area_per_radian = (adjusted_points ++ [hd(adjusted_points)]) |>
      Enum.chunk_every(2, 1, :discard) |>
      Enum.flat_map(fn [{x1, y1}, {x2, y2}] ->
        [a1, a2] = Enum.sort([:math.atan2(y1,x1), :math.atan2(y2,x2)])

        area_per_rad = (x1 * y2 - x2 * y1) / 2 / (a2-a1)
        [
          {a1, area_per_rad},
          {a2, -area_per_rad}
        ]
      end) |>
      Enum.group_by(&elem(&1,0)) |>
      Enum.map(fn {k, v} ->
        sum = v |>
          Enum.map(& elem(&1, 1)) |>
          Enum.reduce(0, &+/2)
        {k, sum}
      end) |>
      Enum.sort |>
      Enum.scan(fn {angle, area_per_radian}, {_prior_angle, prior_area_per_radian} ->
        {angle, prior_area_per_radian + area_per_radian}
      end)

    # Use that to find the full area
    total_area = start_angle_to_area_per_radian |>
      Enum.chunk_every(2, 1, :discard) |>
      Enum.map(fn [{a1, apr}, {a2, _}] ->
        (a2-a1)*apr
      end) |>
      Enum.reduce(0, &+/2)

    # Find the angle that bisects the full area
    {half_angle, _area} = start_angle_to_area_per_radian |>
      Enum.chunk_every(2, 1, :discard) |>
      Enum.reduce_while({0, 0}, fn [{a1, area_per_radian1}, {a2, _area_per_radian2}], {_, subtotal} ->
        next_subtotal = subtotal + (a2-a1)*area_per_radian1
        if next_subtotal <= total_area/2 do
          {:cont, {a2, next_subtotal}}
        else
          ## THIS IS WRONG!!
          ##
          ## It assumes that we can take a percent of the area that we need and then
          ## take a percent of the angle to get that area... but that doesn't work
          ## because triangles aren't pie-peice shaped, so the area isn't smoothly
          ## distributed across the angle.
          ##
          ## By this point, we've lost the points, so we can't pythogreate the answer,
          ## and area_per_radian is acutally useless because the whole point of it
          ## was to do this thing here.
          ##
          percent = (total_area/2 - subtotal) / ((a2 - a1)*area_per_radian1)
          angle = a1 + (a2-a1)*percent
          {:halt, {angle, subtotal + (angle - a1)*area_per_radian1}}
        end
      end)

    {{x_offset, y_offset}, half_angle}
  end

  def solve(points) do
    # Find two lines that bisect the shape
    {{x1, y1}, a1} = bisect(points, :min, :min)
    {{x2, y2}, a2} = bisect(points, :max, :min)

    m1 = :math.tan(a1)
    m2 = :math.tan(a2)

    # Find the point on both lines
    x = (m1 * x1 - y1 - m2 * x2 + y2) / (m1 - m2)
    y = m1 * (x - x1) + y1
    {x, y}
  end

  def process_case do
    n = read_integer()
    points = IO.stream(:stdio, :line) |> Stream.take(n) |> Stream.map(&Point.parse/1) |> Enum.to_list
    {x, y} = solve(points)
    IO.puts "#{:erlang.float_to_binary(x, decimals: 2)} #{:erlang.float_to_binary(y, decimals: 2)}"
  end

  def run do
    cases = read_integer()
    repeat(cases, &process_case/0)
  end

end

Stone.run
