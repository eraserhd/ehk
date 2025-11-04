using System;
using System.Linq;

struct Point
{
    public int X, Y;
    public Point(int x, int y) { X = x; Y = y; }

    public static Point operator -(Point a, Point b)
        => new Point(a.X - b.X, a.Y - b.Y);
}

struct Triangle
{
    public Point A, B;
    public Triangle(Point a, Point b) { A = a; B = b; }

    public double Area() => A.X * B.Y - B.X * A.Y;
}

class Program
{
    static (Point, Point) origins(Point[] points)
    {
        var minX = points.Min(p => p.X) - 10;
        var minY = points.Min(p => p.Y) - 10;
        var maxY = points.Max(p => p.Y) + 10;
        return (new Point(minX, minY), new Point(minX, maxY));
    }

    static double Slope(Point[] points)
    {
        Triangle[] triangles = points
            .Zip(points.Skip(1).Concat(new Point[]{ points[0] }))
            .Select(ps => new Triangle(ps.First, ps.Second))
            .ToArray();

        foreach (var t in triangles)
        {
           Console.WriteLine($"  triangle = ({t.A.X}, {t.A.Y}), ({t.B.X}, {t.B.Y})  --> {t.Area()}");
        }

        var area = triangles
            .Select(t => t.Area())
            .Sum();

        Console.WriteLine($"area = {area}");


        return 1.0;
    }

    static Point Solve(Point[] points)
    {
        var (o1, o2) = origins(points);
        var m1 = Slope(points.Select(p => p - o1).ToArray());
        var m2 = Slope(points.Select(p => p - o2).ToArray());
        return new Point(0, 0);
    }

    static void Main(string[] args)
    {
        int T = int.Parse(Console.ReadLine());
        for (int t = 0; t < T; t++)
        {
            int N = int.Parse(Console.ReadLine());
            Point[] points = new Point[N];
            for (int i = 0; i < N; i++)
            {
                int[] elts = Console.ReadLine().Split().Select(int.Parse).ToArray();
                points[i] = new Point(elts[0], elts[1]);
            }

            Point ans = Solve(points);
            Console.WriteLine($"{ans.X} {ans.Y}");
        }
    }
}
