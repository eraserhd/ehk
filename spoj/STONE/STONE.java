
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
		return (a.y + b.y) * (a.x - b.x) / 2;
	}

	static Triangle[] fromPoints(Point2D.Double[] points) {
		Triangle[] triangles = new Triangle[points.length];
		for (int i = 0; i < points.length; i++) {
			triangles[i] = new Triangle(points[i], points[(i + 1) % points.length]);
		}
		return triangles;
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
}