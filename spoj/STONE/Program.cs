using System;
using System.Linq;

struct Point
{
    public int X, Y;
    public Point(int x, int y) { X = x; Y = y; }

    public Point Adjust(int xd, int yd) => new Point(X+xd, Y+yd);
}

class Program
{
    static Point Solve(Point[] points)
    {
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
