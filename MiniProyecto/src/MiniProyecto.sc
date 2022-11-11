// FUNCIONES

val f1 = (x: Double) => -Math.pow(x, 2) + (8 * x) - 12
val f2 = (x: Double) => 3 * Math.pow(x, 2)
val f3 = (x: Double) => x + (2 * Math.pow(x, 2)) - Math.pow(x, 3) + (5 * Math.pow(x, 4))
val f4 = (x: Double) => (2 * x + 1) / (Math.pow(x, 2) + x)
val f5 = (x: Double) => Math.pow(Math.E, x)
val f6 = (x: Double) => 1 / Math.sqrt(x - 1)
val f7 = (x: Double) => 1 / (1 + Math.pow(x, 2))

def error(a: Double, integracion: Double) = {
  Math.abs(integracion - a)
}

// SIMPSON 1/3

def simpson(a: Int, b: Int, f: Double => Double): Double = {
  val x = (a + b) / 2.0
  ((b - a) * (f(a) + (4 * f(x)) + f(b))) / 6
}

simpson(3, 5, f1)
simpson(0, 2, f2)
simpson(-1, 1, f3)
simpson(1, 2, f4)
simpson(0, 1, f5)
simpson(2, 3, f6)
simpson(0, 1, f7)

val r1 = error(7.33, simpson(3, 5, f1))
val r2 = error(8, simpson(0, 2, f2))
val r3 = error(3.333, simpson(-1, 1, f3))
val r4 = error(1.09861, simpson(1, 2, f4))
val r5 = error(1.71828, simpson(0, 1, f5))
val r6 = error(0.828427, simpson(2, 3, f6))
val r7 = error(0.785398, simpson(0, 1, f7))

// SIMPSON 1/3 COMPUESTA

def simpsonCompuesta(a: Int, b: Int, n: Int, f: Double => Double): Double = {
  val h = 1.0 * (b - a) / n
  val xj = (j: Double) => a + (j * h)
  val funciones = (j: Double) => f(xj((2 * j) - 2)) + 4 * f(xj((2 * j) - 1)) + f(xj(2 * j))
  val funcionesTotal = (1 to n / 2).map(funciones(_))
  (1 to n / 2).map(funciones(_)).sum * h / 3
}

// integracion f1
simpsonCompuesta(3, 5, 2, f1)
simpsonCompuesta(3, 5, 4, f1)
simpsonCompuesta(3, 5, 40, f1)
simpsonCompuesta(3, 5, 100, f1)
simpsonCompuesta(3, 5, 1000, f1)

// integracion f2
simpsonCompuesta(0, 2, 2, f2)
simpsonCompuesta(0, 2, 4, f2)
simpsonCompuesta(0, 2, 40, f2)
simpsonCompuesta(0, 2, 100, f2)
simpsonCompuesta(0, 2, 1000, f2)

// integracion f3
simpsonCompuesta(-1, 1, 2, f3)
simpsonCompuesta(-1, 1, 4, f3)
simpsonCompuesta(-1, 1, 40, f3)
simpsonCompuesta(-1, 1, 100, f3)
simpsonCompuesta(-1, 1, 1000, f3)

// integracion f4
simpsonCompuesta(1, 2, 2, f4)
simpsonCompuesta(1, 2, 4, f4)
simpsonCompuesta(1, 2, 40, f4)
simpsonCompuesta(1, 2, 100, f4)
simpsonCompuesta(1, 2, 1000, f4)

// integracion f5
simpsonCompuesta(0, 1, 2, f5)
simpsonCompuesta(0, 1, 4, f5)
simpsonCompuesta(0, 1, 40, f5)
simpsonCompuesta(0, 1, 100, f5)
simpsonCompuesta(0, 1, 1000, f5)

// integracion f6
simpsonCompuesta(2, 3, 2, f6)
simpsonCompuesta(2, 3, 4, f6)
simpsonCompuesta(2, 3, 40, f6)
simpsonCompuesta(2, 3, 100, f6)
simpsonCompuesta(2, 3, 1000, f6)

// integracion f7
simpsonCompuesta(0, 1, 2, f7)
simpsonCompuesta(0, 1, 4, f7)
simpsonCompuesta(0, 1, 40, f7)
simpsonCompuesta(0, 1, 100, f7)
simpsonCompuesta(0, 1, 1000, f7)

val r2_1 = error(7.33, simpsonCompuesta(3, 5, 1000, f1))
val r2_2 = error(8, simpsonCompuesta(0, 2, 1000, f2))
val r2_3 = error(3.333, simpsonCompuesta(-1, 1, 1000, f3))
val r2_4 = error(1.09861, simpsonCompuesta(1, 2, 1000, f4))
val r2_5 = error(1.71828, simpsonCompuesta(0, 1, 1000, f5))
val r2_6 = error(0.828427, simpsonCompuesta(2, 3, 1000, f6))
val r2_7 = error(0.785398, simpsonCompuesta(0, 1, 1000, f7))

// SIMPSON 1/3 EXTENDIDA

def integracion3(a: Int, b: Int, f: Double => Double): Double = {
  val n = 2 * (b - a)
  val h = 1.0 * (b - a) / n
  val func = (j: Double) => a + (j * h)
  val funciones = f(a) +
    (4 * (1 to n - 1 by 2).map(func(_)).sum) +
    (2 * (2 to n - 2 by 2).map(func(_)).sum) +
    f(b)
  1.0 * (funciones * h) / 3
}

integracion3(3,5,f1)

integracion3(0,2,f2)

integracion3(-1,1,f3)

integracion3(1,2,f4)

integracion3(0,1,f5)

integracion3(2,3,f6)

integracion3(0,1,f7)

val r3_1 = error(7.33, integracion3(3,5,f1))
val r3_2 = error(8, integracion3(0,2,f2))
val r3_3 = error(3.333, integracion3(-1,1,f3))
val r3_4 = error(1.09861, integracion3(1,2,f4))
val r3_5 = error(1.71828, integracion3(0,1,f5))
val r3_6 = error(0.828427, integracion3(2,3,f6))
val r3_7 = error(0.785398, integracion3(0,1,f7))
