import static org.junit.jupiter.api.Assertions.*;

import java.awt.geom.Point2D;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

class STONETest {
	@Test
	void Can_compute_signed_area() {
		var t1 = new Triangle(new Point2D.Double(0,5), new Point2D.Double(3,0));
		assertEquals(0.5*5*3, Math.abs(t1.signedArea()));
		var t2 = new Triangle(new Point2D.Double(3,0), new Point2D.Double(0,5));
		assertEquals(-t1.signedArea(), t2.signedArea());
	}
	
	@Test
	void Can_make_triangles_from_points() {
		var ts = Triangle.fromPoints(new Point2D.Double[] {
				new Point2D.Double(-5,-5),
				new Point2D.Double(-5, 5),
				new Point2D.Double( 5, 5),
				new Point2D.Double( 5,-5),
		});
		var area = Arrays.stream(ts).mapToDouble(Triangle::signedArea).sum();
	    assertEquals(100.0, Math.abs(area));
	}
	
	@Test
	void moveOrigin_moves_points_to_honor_origin() {
		var points = new Point2D.Double[] {
			new Point2D.Double(10,5),
			new Point2D.Double(4, 6),
			new Point2D.Double(-2, -2),
		};
		var origin = new Point2D.Double(5,5);
		var adjusted = PointUtils.moveOrigin(origin, points);
		assertEquals(new Point2D.Double(5,0), adjusted[0]);
		assertEquals(new Point2D.Double(-1,1), adjusted[1]);
		assertEquals(new Point2D.Double(-7,-7), adjusted[2]);
	}
}