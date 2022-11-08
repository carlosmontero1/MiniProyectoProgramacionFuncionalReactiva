val f = (x: Double) => -Math.pow(x, 2) + (8 * x) - 12

f(2)
f(1)

def integracion(a: Int, b: Int, f: Double => Double): Double = {
  val x = (a + b) / 2.0
  ((b - a) * (f(a) + (4 * f(x)) + f(b))) / 6
}

integracion(3, 5, f)

val f2 = (x: Double) => 3 * (Math.pow(x, 2))
integracion(0, 2, f2)

val f3 = (x: Double) => x + (2 * Math.pow(x, 2)) - (Math.pow(x, 3)) + (5 * (Math.pow(x, 4)))
integracion(-1, 1, f3)

val f4 = (x: Double) => (2 * x + 1) / (Math.pow(x, 2) + x)
integracion(1, 2, f4)

val f5 = (x: Double) => Math.pow(Math.E, x)
integracion(0, 1, f5)

val f6 = (x: Double) => 1 / Math.sqrt((x - 1))
integracion(2, 3, f6)

val f7 = (x: Double) => 1 / (1 + Math.pow(x, 2))
integracion(0, 1, f7)

def error(a: Double, integracion: Double) = {
  Math.abs(integracion - a)
}

val r1 = error(7.33, integracion(3, 5, f))
val r2 = error(8, integracion(0, 2, f2))
val r3 = error(3.333, integracion(-1, 1, f3))
val r4 = error(1.09861, integracion(1, 2, f4))
val r5 = error(1.71828, integracion(0, 1, f5))
val r6 = error(0.828427, integracion(2, 3, f6))
val r7 = error(0.785398, integracion(0, 1, f7))
