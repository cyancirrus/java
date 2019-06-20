// this short exercise written to work on https://scalafiddle.io/   //


def absoluteValue( value: Int ): Int = {
  if ( value < 0 ) return - value
  else return value
}


class Date{
  /*******************************/
  /* attributes and definitions  */
  /*******************************/
  
  private var _year   = 0
  private var _month  = 0
  private var _day    = 0
  private var _value  = 0
  
  
  def year  = _year
  def month = _month
  def day   = _day
  def value = _value
  
  /*******************************/
  /*constructs for initialization*/
  /*******************************/
  
  def year_= ( yearValue: Int ): Unit = {
    _year = yearValue
  }
  
  def month_= ( monthValue: Int ): Unit = {
    if ( 0<= monthValue & monthValue <= 12 ) _month = monthValue
      else printWarningMonth
  }
  
  def day_= ( dayValue: Int ): Unit = {
    if ( 0 <= dayValue & dayValue <= 31 ) _day = dayValue
      else printWarningDay
  }
  
  def value_= ( initialize: Unit ): Unit = {
    _value =
      this.year * 365 +
      this.month * 31 +
      this.day
  }
  
  def construct( yearValue: Int, monthValue: Int, dayValue: Int ): Unit = {
    this.year   = yearValue
    this.month  = monthValue
    this.day    = dayValue
    
    this.value  = ()
  }
  // no leading "_" like _methodname so _methodName called with = value
  // normal methods called with parameters as standard
  
  /*******************************/
  /*      basic date functions   */
  /*******************************/
  
  /********** print **************/
  
  def display: Unit = {
    println(
      this.year.toString + "/" +
      this.month.toString + "/" +
      this.day
      )
  }
  
  /****** date add ****************/
  
  private def dayAdd( dayValue: Int ): Unit = {
    var totalDays: Int = this.day + dayValue
      if ( this.day != totalDays ) this.day = totalDays%31
      
    var totalMonths: Int = this.month + ( totalDays / 31 ).toInt
      if ( this.month != totalMonths ) this.month = totalMonths%12
      
    var totalYears: Int = this.year + ( totalMonths / 12 ).toInt
      if ( this.year != totalYears ) this.year = totalYears
  }
  
  private def monthAdd( monthValue: Int ): Unit = {
    var totalMonths: Int = ( this.month + monthValue )
      if ( this.month != totalMonths ) this.month = totalMonths%12
    
    var totalYears: Int = this.year + ( totalMonths / 12 ).toInt
      if ( this.year != totalYears ) this.year = totalYears
  }
  
  private def yearAdd( yearValue: Int ): Unit = {
    if ( yearValue != 0 ) this.year = this.year + yearValue
  }
  
  // setting private to avoid calling function without updates to this.value //
  
  def dateAdd( incrementType: String, incrementValue: Int ): Unit = {
    incrementType match{
      case "year"   =>  yearAdd( incrementValue )
      case "month"  =>  monthAdd( incrementValue )
      case "day"    =>  dayAdd( incrementValue )
    }
    
    incrementType match{
      case "year"   =>  this.value += incrementValue * 365
      case "month"  =>  this.value += incrementValue * 31
      case "day"    =>  this.value += incrementValue
    }
  }
  
  /******** date dif *************/
  
  def difPrimative( secondDate: Date ): Int = {
    this.value - secondDate.value
  }
  
  def dateDifference( measurementType: String, secondDate: Date ): Int = {
    var dif_prim: Int = absoluteValue( difPrimative( secondDate ) )
    
    /* simple method for getting dates but cannot be extended beyond uniform months */
    
    /*
    measurementType match{
      case "year"   =>  ( dif_prim / 365 ).toInt
      case "month"  =>  ( dif_prim / 12 ).toInt
      case "day"    =>  dif_prim
    }
    */
    
    var delta: Date = new Date
      delta.dateAdd( "day", dif_prim )
      // this method would have to be cleaned up for dateDifference to give exact results //
      
    measurementType match{
      case "year" =>
        delta.year
      case "month" =>
        delta.year * 12 +
        delta.month
      case "day" =>
        delta.year * 365 +
        delta.month * 12 +
        delta.day
    }
  }

  def printWarningYear = println( "warning: not a valid year" )
  def printWarningMonth = println( "warning: domain month [1,12]")
  def printWarningDay = println( "warning: max domain day [0,31]")
}





////////////////////////
/// TESTING CODE     ///
////////////////////////


var dateNow = new Date

dateNow.construct( 2019, 6, 20 )

dateNow.value

dateNow.dateAdd( "month", 13 )

var dateOther = new Date

dateOther.construct( 2014, 3, 20 )


println( dateOther.dateDifference( "day", dateNow ) )

dateOther.display
dateNow.display

