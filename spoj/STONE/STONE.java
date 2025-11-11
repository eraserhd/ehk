
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

class PointUtils {
	static Point2D.Double[] moveOrigin(Point2D.Double origin, Point2D.Double[] points) {
		Point2D.Double[] result = new Point2D.Double[points.length];
		for (int i = 0; i < points.length; i++) {
			result[i] = new Point2D.Double(points[i].getX() - origin.getX(), points[i].getY() - origin.getY());
		}
		return result;
	}
}