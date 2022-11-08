// SIMPSON 1/3

def integracion(a: Int, b: Int, f: Double => Double): Double = {
  val x = (a + b) / 2.0
  ((b - a) * (f(a) + (4 * f(x)) + f(b))) / 6
}

val f = (x: Double) => -Math.pow(x, 2) + (8 * x) - 12
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


// SIMPSON 1/3 COMPUESTA

def integracion2(a: Int, b: Int, n: Int, f: Double => Double): Double = {
  val h = 1.0 * (b - a) / n
  val xj = (j: Double) => a + (j*h)
  val funciones = (j: Double) => f(xj((2*j)-2)) + 4*f(xj((2*j)-1)) + f(xj(2*j))
  val funcionesTotal = (1 to n/2).map(funciones(_))
  (1 to n/2).map(funciones(_)).sum * h/3
}

// integracion f1
integracion2(3,5,2,f)
integracion2(3,5,4,f)
integracion2(3,5,40,f)
integracion2(3,5,100,f)
integracion2(3,5,1000,f)

// integracion f2
integracion2(0,2,2,f2)
integracion2(0,2,4,f2)
integracion2(0,2,40,f2)
integracion2(0,2,100,f2)
integracion2(0,2,1000,f2)

// integracion f3
integracion2(-1, 1, 2, f3)
integracion2(-1, 1, 4, f3)
integracion2(-1, 1, 40, f3)
integracion2(-1, 1, 100, f3)
integracion2(-1, 1, 1000, f3)

// integracion f4
integracion2(1,2,2,f4)
integracion2(1,2,4,f4)
integracion2(1,2,40,f4)
integracion2(1,2,100,f4)
integracion2(1,2,1000,f4)

// integracion f5
integracion2(0,1,2,f5)
integracion2(0,1,4,f5)
integracion2(0,1,40,f5)
integracion2(0,1,100,f5)
integracion2(0,1,1000,f5)

// integracion f6
integracion2(2,3,2,f6)
integracion2(2,3,4,f6)
integracion2(2,3,40,f6)
integracion2(2,3,100,f6)
integracion2(2,3,1000,f6)

// integracion f7
integracion2(0,1,2,f7)
integracion2(0,1,4,f7)
integracion2(0,1,40,f7)
integracion2(0,1,100,f7)
integracion2(0,1,1000,f7)

val res1 = error(7.33, integracion2(3,5,1000,f))
val res2 = error(8, integracion2(0,2,1000,f2))
val res3 = error(3.333, integracion2(-1, 1, 1000, f3))
val res4 = error(1.09861, integracion2(1,2,1000,f4))
val res5 = error(1.71828, integracion2(0,1,1000,f5))
val res6 = error(0.828427, integracion2(2,3,1000,f6))
val res7 = error(0.785398, integracion2(0,1,1000,f7))

