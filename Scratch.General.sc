
val ma = (0 to 9).map(x => (x, x)).toMap
val mb = (0 to 9).map(x => (x, x + 10)).toMap


val m1 = Map(0 -> 0, 1 -> 0)
val m2 = m1 +(2->0)
val m3 = m2 +(1->3)

val l = 1 to 3 toList
val l2 = 0 until 3 toList

val v = Vector(1,2,3,4)
v.length

v(0)

val x = v.lift(0)
identity(2)

// val y = x.fold(4)(_=>_)

// Some(1).fold(Some(3))(_ => _)


val v1 = Vector(1,2,3)
val v2 = Vector(10, 11)
val v3 = v1.zip(v2)

