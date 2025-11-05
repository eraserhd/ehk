
import java.util.Arrays;
import java.util.Objects;
import java.util.Scanner;
import java.util.stream.IntStream;

class Point {
	double x, y;

	Point(double x, double y) {
		this.x = x;
		this.y = y;
	}

	Point subtract(Point other) {
		return new Point(x - other.x, y - other.y);
	}

	double angle() {
		return Math.atan2(y, x);
	}

	@Override
	public int hashCode() {
		return Objects.hash(x, y);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Point other = (Point) obj;
		return Double.doubleToLongBits(x) == Double.doubleToLongBits(other.x)
				&& Double.doubleToLongBits(y) == Double.doubleToLongBits(other.y);
	}

	@Override
	public String toString() {
		return "Point [x=" + x + ", y=" + y + "]";
	}
}

class Triangle {
	Point p1, p2;

	Triangle(Point p1, Point p2) {
		this.p1 = p1;
		this.p2 = p2;
	}

	double area() {
		return (p1.x * p2.y - p2.x * p1.y) / 2;
	}

	static boolean isLeftToRight(double a1, double a2) {
		return a2 < a1;
	}

	double[] leftAndRightArea(double angle) {
		final double[] result = new double[] { 0, 0 };
		if (isLeftToRight(p1.angle(), angle) && isLeftToRight(p2.angle(), angle))
			result[0] += area();
		else if (isLeftToRight(angle, p1.angle()) && isLeftToRight(angle, p2.angle()))
			result[1] += area();
		else {
			
			double slope = Math.tan(angle);
		    
		    // Solve for t where the bisecting line intersects the edge
		    // p1.y + t(p2.y - p1.y) = slope * (p1.x + t(p2.x - p1.x))
		    // Rearranging: t = (slope * p1.x - p1.y) / ((p2.y - p1.y) - slope * (p2.x - p1.x))
		    
		    double numerator = slope * p1.x - p1.y;
		    double denominator = (p2.y - p1.y) - slope * (p2.x - p1.x);
		    
		    double t = numerator / denominator;
		    
		    // The intersection point
		    final Point midpoint = new Point(
		        p1.x + t * (p2.x - p1.x),
		        p1.y + t * (p2.y - p1.y)
		    );
		    		    
		    result[0] += new Triangle(p1,midpoint).area();
		    result[1] += new Triangle(midpoint,p2).area();
		}
		return result;
	}
}

public class STONE {
	static double bisectingSlope(final Point[] points) {
		final Triangle[] triangles = IntStream.range(0, points.length - 1)
				.mapToObj(i -> new Triangle(points[i], points[(i + 1) % points.length])).toArray(Triangle[]::new);

		//debug
		final double area = Arrays.stream(triangles).mapToDouble(Triangle::area).sum();
		System.out.println("total area = "+area);
		
		final boolean quadrant1 = points[0].x > 0;
		double rightSide = quadrant1 ? 0.0 : Math.PI/2;
		double leftSide = quadrant1 ? Math.PI/2 : Math.PI;
		double middle = (rightSide + leftSide)/2;
		
		int iterations = 25;
		while (rightSide < leftSide) {
			middle = (rightSide + leftSide) / 2;

			double leftArea = 0.0;
			double rightArea = 0.0;

			for (Triangle t : triangles) {
				final double[] lr = t.leftAndRightArea(middle);
				leftArea += lr[0];
				rightArea += lr[1];
			}
			
			if (Math.abs(area - (leftArea + rightArea)) > 0.1) {
				System.out.println("FAIL");
				System.out.println("middle="+middle);
				System.exit(1);
			}
			
			System.out.println("==iteration"+iterations);
			System.out.println("leftSide = "+leftSide+", leftArea = "+leftArea);
			System.out.println("rightSide = "+rightSide+", rightArea = "+rightArea);

			if (Math.abs(leftArea) < Math.abs(rightArea)) {
				leftSide = middle;
			} else {
				rightSide = middle;
			}
			
			if (--iterations == 0) {
				break;
			}
		}

		return Math.tan(middle);
	}

	static Point solve(Point[] points) {
		final double minX = Arrays.stream(points).mapToDouble(p -> p.y).min().getAsDouble();
		final double maxX = Arrays.stream(points).mapToDouble(p -> p.x).min().getAsDouble();
		final double minY = Arrays.stream(points).mapToDouble(p -> p.y).min().getAsDouble();

		final Point origin1 = new Point(minX - 100, minY - 100);
		final Point[] points1 = Arrays.stream(points).map(p -> p.subtract(origin1)).toArray(Point[]::new);
		final double slope1 = bisectingSlope(points1);

		final Point origin2 = new Point(maxX + 100, minY - 100);
		final Point[] points2 = Arrays.stream(points).map(p -> p.subtract(origin2)).toArray(Point[]::new);
		final double slope2 = bisectingSlope(points2);

		// y - y1 = m(x - x1) // point-slope
		// y = slope1*(x - origin1.x) + origin1.y
		// y = slope2*(x - origin2.x) + origin2.y

		// slope1*(x - origin1.x) + origin1.y = slope2*(x - origin2.x) + origin2.y
		// slope1*x - slope1*origin1.x + origin1.y = slope2*x - slope2*origin2.x +
		// origin2.y
		// slope1*x - slope2.x = slope1*origin1.x - origin1.y - slope2*origin2.x +
		// origin2.y
		// x*(slope1 - slope2) = ...

		final double x = (slope1 * origin1.x - origin1.y - slope2 * origin2.x + origin2.y) / (slope1 - slope2);
		final double y = slope1 * (x - origin1.x) + origin1.y;
		return new Point(x, y);
	}

	public static void main(String[] args) {
		final Scanner sc = new Scanner(System.in);
		final int T = sc.nextInt();
		for (int i = 0; i < T; i++) {
			final int N = sc.nextInt();
			final Point[] points = new Point[N];
			for (int j = 0; j < N; j++) {
				final int X = sc.nextInt();
				final int Y = sc.nextInt();
				points[j] = new Point(X, Y);
			}
			final Point result = solve(points);
			System.out.println("" + result.x + " " + result.y);
		}
		sc.close();
	}
}
