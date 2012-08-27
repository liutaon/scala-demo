package interview

abstract case class Demo() {
    private val pattern = """[^\d]+(\d+).*""".r
    def index = {
        getClass.getName match {
            case pattern(id) => id.toInt.toString
            case o@_ => o
        }
    }
    def test(): Unit;
}

object Interview extends App {
    import scala.collection.mutable
    val demos = mutable.Map[String, Demo]()
    // add demos
    add(I001, I002, I003, I004, I005, I006, I007, I008, I009, I010,
        I011, I012, I013, I014, I015, I016, I017, I018, I019, I020,
        I021)

    def add(all: Demo*) = all.foreach { d => demos += d.index -> d}

    args.foreach { a =>
        demos.get(a) match {
            case Some(d) => println("==== Run No." + d.index +" ===="); d.test()
            case _ => println("Not found " + a)
        }
    }
}