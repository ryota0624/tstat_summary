case class Time(value: String)

class Usr(val value: Float) extends AnyVal {
  def +(that: Usr): Usr = new Usr(this.value + that.value)
  def /(intNumber: Int): Usr = {
    val value = if (intNumber != 0) this.value / intNumber else 0
    new Usr(value)
  }
}

object Usr {
  def apply(str: String): Usr = new Usr(str.toFloat)
}

class Sys(val value: Float) extends AnyVal {
  def +(that: Sys): Sys = new Sys(this.value + that.value)
  def /(intNumber: Int): Sys = {
    val value = if (intNumber != 0) this.value / intNumber else 0
    new Sys(value)
  }
}

object Sys {
  def apply(str: String): Sys = new Sys(str.toFloat)
}

class Idl(val value: Float) extends AnyVal {
  def +(that: Idl): Idl = new Idl(this.value + that.value)
  def /(intNumber: Int): Idl = {
    val value = if (intNumber != 0) this.value / intNumber else 0
    new Idl(value)
  }
}

object Idl {
  def apply(str: String): Idl = new Idl(str.toFloat)
}

class Wai(val value: Float) extends AnyVal {
  def +(that: Wai): Wai = new Wai(this.value + that.value)
  def /(intNumber: Int): Wai = {
    val value = if (intNumber != 0) this.value / intNumber else 0
    new Wai(value)
  }
}

object Wai {
  def apply(str: String): Wai = new Wai(str.toFloat)
}

class Hiq(val value: Float) extends AnyVal {
  def +(that: Hiq): Hiq = new Hiq(this.value + that.value)
  def /(intNumber: Int): Hiq = {
    val value = if (intNumber != 0) this.value / intNumber else 0
    new Hiq(value)
  }
}

object Hiq {
  def apply(str: String): Hiq = new Hiq(str.toFloat)
}

class Siq(val value: Float) extends AnyVal {
  def +(that: Siq): Siq = new Siq(this.value + that.value)
  def /(intNumber: Int): Siq = {
    val value = if (intNumber != 0) this.value / intNumber else 0
    new Siq(value)
  }
}

object Siq {
  def apply(str: String): Siq = new Siq(str.toFloat)
}

sealed trait LoadAvg extends Ordered[LoadAvg] {
  val value: Float
  val time: Int
  override def compare(that: LoadAvg): Int = {
    LoadAvg.ordering.compare(this, that)
  }

  override def toString: String = value.toString
}

object LoadAvg {
  def allTimeLAFromString(str: String): (LoadAvg1, LoadAvg5, LoadAvg15) = {
    val values = str.split(""" """).filter(_.length > 0).map(_.toFloat)
    (LoadAvg1(values(0)), LoadAvg5(values(1)), LoadAvg15(values(2)))
  }


  def maxLoadAverage[L <: LoadAvg](loadAverages: Seq[L]): L = loadAverages.max[LoadAvg]
  def minLoadAverage[L <: LoadAvg](loadAverages: Seq[L]): L = loadAverages.min[LoadAvg]

  def edgeAverage[L <: LoadAvg](loadAverages: Seq[L]): (L, L) = (minLoadAverage(loadAverages), maxLoadAverage(loadAverages))


  val ordering: Ordering[LoadAvg] = Ordering.by(_.value)
}

case class LoadAvg1(value: Float) extends LoadAvg {
  val time = 1
}

case class LoadAvg5(value: Float) extends LoadAvg {
  val time = 5
}

case class LoadAvg15(value: Float) extends LoadAvg {
  val time = 15
}

case class CpuUsage(
                     usr: Usr,
                     sys: Sys,
                     idl: Idl,
                     wai: Wai,
                     hiq: Hiq,
                     siq: Siq,
                   ) {

  def /(intNumber: Int): CpuUsage = {
    new CpuUsage(
      usr / intNumber,
      sys / intNumber,
      idl / intNumber,
      wai / intNumber,
      hiq / intNumber,
      siq / intNumber
    )
  }

  override def toString: String = {
    s"${usr.value}, ${sys.value}, ${idl.value}, ${wai.value}, ${hiq.value}, ${siq.value}\n"
  }

  def +(that: CpuUsage): CpuUsage = CpuUsage(
    usr = this.usr + that.usr,
    sys = this.sys + that.sys,
    idl = this.idl + that.idl,
    wai = this.wai + that.wai,
    hiq = this.hiq + that.hiq,
    siq = this.siq + that.siq)
}

object CpuUsage {
  def apply(str: String): CpuUsage = {
    val stringRecord = str.split(""" """).filter(_.length > 0)
    val usr = stringRecord(0)
    val sys = stringRecord(1)
    val idl = stringRecord(2)
    val wai = stringRecord(3)
    val hiq = stringRecord(4)
    val siq = stringRecord(5)
    new CpuUsage(Usr(usr), Sys(sys), Idl(idl), Wai(wai), Hiq(hiq), Siq(siq))
  }

  def cpuUsageAverage(cpuUsages: Seq[CpuUsage]): CpuUsage = {
    cpuUsages.reduce(_ + _) / cpuUsages.length
  }
}


case class Record(
                   time: Time,
                   cpuUsage: CpuUsage,
                   loadAvg1: LoadAvg1,
                   loadAvg5: LoadAvg5,
                   loadAvg15: LoadAvg15
                 )

object Record {
  def apply(str: String): Record = {
    val recordStrings = str.split("""\|""")
    val time = Time(recordStrings(0))
    val cpuUsage = CpuUsage(recordStrings(1))
    val (loadAvg1, loadAvg5, loadAvg15) = LoadAvg.allTimeLAFromString(recordStrings(6))

    new Record(time, cpuUsage, loadAvg1, loadAvg5, loadAvg15)
  }
}