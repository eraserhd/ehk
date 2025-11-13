
import java.awt.geom.Point2D;
import java.util.Arrays;

class Triangle {
	Point2D.Double a;
	Point2D.Double b;

	Triangle(Point2D.Double a, Point2D.Double b) {
		this.a = a;
		this.b = b;
	}

	double signedArea() {
		return (a.y + b.y) * (a.x - b.x) / 2.0;
	}

	static Triangle[] fromPoints(Point2D.Double[] points) {
		Triangle[] triangles = new Triangle[points.length];
		for (int i = 0; i < points.length; i++) {
			triangles[i] = new Triangle(points[i], points[(i + 1) % points.length]);
		}
		return triangles;
	}
	
	Triangle[] bisect(double slope) {
		// (A) y = slope*x
		// (B) (x - a.x) / (b.x - a.x) = (y - a.y)/(b.y - a.y)
		
		// (x - a.x) / (b.x - a.x) = (slope*x - a.y)/(b.y - a.y)
		// (x - a.x)*(b.y - a.y) = (b.x - a.x)*(slope*x - a.y)
		// x*(b.y - a.y) - a.x*(b.y - a.y) = b.x*(slope*x - a.y) - a.x*(slope*x - a.y)
		// x*b.y - x*a.y - a.x*b.y - a.x*a.y = b.x*slope*x - b.x*a.y - a.x*slope*x - a.x*a.y
		// x*b.y - x*a.y -b.x*slope*x + a.x*slope*x = a.x*b.y + a.x*a.y - b.x*a.y - a.x*a.y
		// x(b.y - a.y - b.x*slope + a.x*slope) = a.x*b.y + a.x*a.y - b.x*a.y - a.x*a.y
		final double x = (b.x - a.x)*(slope*x)
		
		final double y = slope*x;
		final Point2D.Double middle = new Point2D.Double(x, y);
		System.out.println("a = "+a+"; b = "+b+"; middle = "+middle);
		return new Triangle[] {
				new Triangle(a, middle),
				new Triangle(middle, b),
		};
	}
}

class STONE {
	static Point2D.Double[] moveOrigin(Point2D.Double origin, Point2D.Double[] points) {
		Point2D.Double[] result = new Point2D.Double[points.length];
		for (int i = 0; i < points.length; i++) {
			result[i] = new Point2D.Double(points[i].getX() - origin.getX(), points[i].getY() - origin.getY());
		}
		return result;
	}
	
	// Find two origins outside the figure, placing the figure in two separate quadrants.
	// Since we find a slope through the origin bisecting the figure, we can find two
	// lines that bisect the figure from different angles.
	static Point2D.Double[] findOrigins(Point2D.Double[] points) {
		double minY = Arrays.stream(points).mapToDouble(Point2D.Double::getY).min().getAsDouble();
		double minX = Arrays.stream(points).mapToDouble(Point2D.Double::getX).min().getAsDouble();
		double maxX = Arrays.stream(points).mapToDouble(Point2D.Double::getX).max().getAsDouble();
		return new Point2D.Double[] {
				new Point2D.Double(minX - 100, minY - 100),
				new Point2D.Double(maxX + 100, minY - 100),
		};
	}
	
	static Point2D.Double findIntersection(final Point2D.Double p1, final double slope1, final Point2D.Double p2, final double slope2) {
		// y - p1.y1 = slope1*(x - p1.x)
		// y = slope1*(x - p1.x) + p1.y
		// slope2*(x - p2.x) + p2.y = slope1*(x - p1.x) + p1.y
		// slope2*x - slope2*p2.x + p2.y = slope1*x - slope1*p1.x + p1.y
		// slope2*x - slope1*x = slope2*p2.x - p2.y - slope1*p1.x + p1.y
		// (slope2 - slope1)*x = ...
		// x = (slope2*p2.x - p2.y - slope1*p1.x + p1.y)/(slope2 - slope1);
		final double x = (slope2*p2.x - p2.y - slope1*p1.x + p1.y)/(slope2 - slope1);
		final double y = slope1*(x - p1.x) + p1.y;
		return new Point2D.Double(x, y);
	}
	
	/*
	static double areaBisectingSlopeThroughOrigin(Triangle[] triangles) {
		double left = Math.PI/4;
		double right = 0.0;
		while (left > right) {
			double middle = (left + right)/2.0;
			
		
		}
		return Math.tan((left + right)/2.0);
	}*/
}