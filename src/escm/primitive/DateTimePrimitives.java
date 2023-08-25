// Author: Jordan Randleman - escm.primitive.DateTimePrimitives
// Purpose:
//    Java primitives for date/time operations.

package escm.primitive;
import java.util.ArrayList;
import java.util.Calendar;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;

public class DateTimePrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // current-time
  public static class CurrentTime extends Primitive {
    public java.lang.String escmName() {
      return "current-time";
    }

    public Datum signature() {
      return Pair.List(new Symbol("current-time"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(current-time) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      Calendar c = Calendar.getInstance();
      return Pair.List(new Exact(c.get(Calendar.HOUR_OF_DAY)), 
                       new Exact(c.get(Calendar.MINUTE)), 
                       new Exact(c.get(Calendar.SECOND)), 
                       new Exact(c.get(Calendar.MILLISECOND)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-date
  public static class CurrentDate extends Primitive {
    public java.lang.String escmName() {
      return "current-date";
    }

    public Datum signature() {
      return Pair.List(new Symbol("current-date"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(current-date) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      Calendar c = Calendar.getInstance();
      return Pair.List(new Exact(c.get(Calendar.YEAR)), 
                       new Exact(c.get(Calendar.MONTH)+1), 
                       new Exact(c.get(Calendar.DAY_OF_MONTH)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // epoch-time
  public static class EpochTime extends Primitive {
    public java.lang.String escmName() {
      return "epoch-time";
    }

    public Datum signature() {
      return Pair.List(new Symbol("epoch-time"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(epoch-time) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return new Exact(Calendar.getInstance().getTimeInMillis());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // time-zone
  public static class TimeZone extends Primitive {
    public java.lang.String escmName() {
      return "time-zone";
    }

    public Datum signature() {
      return Pair.List(new Symbol("time-zone"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(time-zone) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(Calendar.getInstance().getTimeZone().getDisplayName());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // day
  public static class Day extends Primitive {
    public java.lang.String escmName() {
      return "day";
    }

    public Datum signature() {
      return Pair.List(new Symbol("day"));
    }

    public static final escm.type.String SUNDAY = new escm.type.String("Sunday");
    public static final escm.type.String MONDAY = new escm.type.String("Monday");
    public static final escm.type.String TUESDAY = new escm.type.String("Tuesday");
    public static final escm.type.String WEDNESDAY = new escm.type.String("Wednesday");
    public static final escm.type.String THURSDAY = new escm.type.String("Thursday");
    public static final escm.type.String FRIDAY = new escm.type.String("Friday");
    public static final escm.type.String SATURDAY = new escm.type.String("Saturday");
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(day) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      switch(Calendar.getInstance().get(Calendar.DAY_OF_WEEK)) {
        case Calendar.SUNDAY: return SUNDAY;
        case Calendar.MONDAY: return MONDAY;
        case Calendar.TUESDAY: return TUESDAY;
        case Calendar.WEDNESDAY: return WEDNESDAY;
        case Calendar.THURSDAY: return THURSDAY;
        case Calendar.FRIDAY: return FRIDAY;
        case Calendar.SATURDAY: return SATURDAY;
        default: return Boolean.FALSE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // month
  public static class Month extends Primitive {
    public java.lang.String escmName() {
      return "month";
    }

    public Datum signature() {
      return Pair.List(new Symbol("month"));
    }

    public static final escm.type.String JANUARY = new escm.type.String("January");
    public static final escm.type.String FEBRUARY = new escm.type.String("February");
    public static final escm.type.String MARCH = new escm.type.String("March");
    public static final escm.type.String APRIL = new escm.type.String("April");
    public static final escm.type.String MAY = new escm.type.String("May");
    public static final escm.type.String JUNE = new escm.type.String("June");
    public static final escm.type.String JULY = new escm.type.String("July");
    public static final escm.type.String AUGUST = new escm.type.String("August");
    public static final escm.type.String SEPTEMBER = new escm.type.String("September");
    public static final escm.type.String OCTOBER = new escm.type.String("October");
    public static final escm.type.String NOVEMBER = new escm.type.String("November");
    public static final escm.type.String DECEMBER = new escm.type.String("December");
    public static final escm.type.String UNDECIMBER = new escm.type.String("Undecimber");

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(month) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      switch(Calendar.getInstance().get(Calendar.MONTH)) {
        case Calendar.JANUARY: return JANUARY;
        case Calendar.FEBRUARY: return FEBRUARY;
        case Calendar.MARCH: return MARCH;
        case Calendar.APRIL: return APRIL;
        case Calendar.MAY: return MAY;
        case Calendar.JUNE: return JUNE;
        case Calendar.JULY: return JULY;
        case Calendar.AUGUST: return AUGUST;
        case Calendar.SEPTEMBER: return SEPTEMBER;
        case Calendar.OCTOBER: return OCTOBER;
        case Calendar.NOVEMBER: return NOVEMBER;
        case Calendar.DECEMBER: return DECEMBER;
        case Calendar.UNDECIMBER: return UNDECIMBER;
        default: return Boolean.FALSE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // year
  public static class Year extends Primitive {
    public java.lang.String escmName() {
      return "year";
    }

    public Datum signature() {
      return Pair.List(new Symbol("year"));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(year) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(String.valueOf(Calendar.getInstance().get(Calendar.YEAR)));
    }
  }
}