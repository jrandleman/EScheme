package escm.primitive;

import java.util.ArrayList;
import java.util.HashMap;
import escm.type.Datum;
import escm.type.Nil;
import escm.type.Pair;
import escm.type.Symbol;
import escm.util.error.Exceptionf;
import escm.util.json.JsonArray;
import escm.util.json.JsonBoolean;
import escm.util.json.JsonDatum;
import escm.util.json.JsonNull;
import escm.util.json.JsonNumber;
import escm.util.json.JsonObject;
import escm.util.json.JsonString;
import escm.vm.type.primitive.Primitive;

public class JsonPrimitives {
  public static class JsonToData extends Primitive {
    public java.lang.String escmName() {
      return "json->data";
    }

    public Datum signature() {
      return Pair.List(new Symbol("json->data"), new Symbol("<json-string>"));
    }

    public String docstring() {
      return "@help:Procedures:JSON\nConvert a JSON string to an EScheme data structure.";
    }

    private static Datum convertJsonDatumToEscmDatum(JsonDatum jsonDatum) throws Exception {
      if (jsonDatum instanceof JsonArray) {
        ArrayList<Datum> escmList = new ArrayList<Datum>();
        ArrayList<JsonDatum> jsonList = ((JsonArray) jsonDatum).value();
        for (int i = 0; i < jsonList.size(); i++)
          escmList.add(convertJsonDatumToEscmDatum(jsonList.get(i)));
        return new escm.type.Vector(escmList);
      } else if (jsonDatum instanceof JsonObject) {
        HashMap<String, JsonDatum> value = ((JsonObject) jsonDatum).value();
        escm.type.Hashmap escmMap = new escm.type.Hashmap();
        for (String key : value.keySet())
          escmMap.set(new escm.type.String(key), convertJsonDatumToEscmDatum(value.get(key)));
        return escmMap;
      } else if (jsonDatum instanceof JsonBoolean) {
        return escm.type.bool.Boolean.valueOf(((JsonBoolean) jsonDatum) == JsonBoolean.TRUE);
      } else if (jsonDatum instanceof JsonNull) {
        return Nil.VALUE;
      } else if (jsonDatum instanceof JsonNumber) {
        return new escm.type.number.Inexact((((JsonNumber) jsonDatum).value()));
      } else if (jsonDatum instanceof JsonString) {
        return new escm.type.String(((JsonString) jsonDatum).value());
      } else {
        throw new Exceptionf("'(json->data <json-string>) invalid JSON: %s", jsonDatum);
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if (parameters.size() != 1)
        throw new Exceptionf("'(json->data <json-string>) invalid arg signature: %s",
            Exceptionf.profileArgs(parameters));
      Datum jsonStringDatum = parameters.get(0);
      if (!(jsonStringDatum instanceof escm.type.String))
        throw new Exceptionf("'(json->data <json-string>) arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      String jsonString = ((escm.type.String) jsonStringDatum).value();
      try {
        return convertJsonDatumToEscmDatum(JsonDatum.parse(jsonString));
      } catch (Exception e) {
        throw new Exceptionf("'(json->data <json-string>) error (%s) parsing invalid JSON: %s", e.getMessage(),
            jsonString);
      }
    }
  }

  public static class DataToJson extends Primitive {
    public java.lang.String escmName() {
      return "data->json";
    }

    public Datum signature() {
      return Pair.List(Pair.List(new Symbol("data->json"), new Symbol("<json-compliant-datum>")),
          Pair.List(new Symbol("data->json"), new Symbol("<json-compliant-datum>"),
              new Symbol("<tab-width-integer>")));
    }

    public String docstring() {
      return "@help:Procedures:JSON\nConvert a JSON-compliant EScheme datum to a JSON string. JSON-compliant data\ninclude booleans, nils, numbers, and strings, as well as vectors and hashmaps\nthereof (so long as the hashmap keys are all strings).";
    }

    private static JsonDatum convertEscmDatumToJsonDatum(Datum datum) throws Exception {
      if (datum instanceof escm.type.Vector) {
        escm.type.Vector vector = (escm.type.Vector) datum;
        ArrayList<JsonDatum> jsonList = new ArrayList<JsonDatum>();
        for (int i = 0; i < vector.size(); i++)
          jsonList.add(convertEscmDatumToJsonDatum(vector.get(i)));
        return new JsonArray(jsonList);
      } else if (datum instanceof escm.type.Hashmap) {
        escm.type.Hashmap hashmap = (escm.type.Hashmap) datum;
        HashMap<String, JsonDatum> jsonMap = new HashMap<String, JsonDatum>();
        Datum keys = hashmap.keys();
        while (keys instanceof Pair) {
          Pair p = (Pair) keys;
          Datum keyDatum = p.car();
          if (!(keyDatum instanceof escm.type.String))
            throw new Exceptionf(
                "'(data->json <json-compliant-datum> <optional-tab-width>) hashmap key isn't a string: %s",
                keyDatum.profile());
          String key = ((escm.type.String) keyDatum).value();
          jsonMap.put(key, convertEscmDatumToJsonDatum(hashmap.get(keyDatum)));
          keys = p.cdr();
        }
        return new JsonObject(jsonMap);
      } else if (datum instanceof escm.type.String) {
        return new JsonString(((escm.type.String) datum).value());
      } else if (datum instanceof escm.type.number.Number) {
        if (datum instanceof escm.type.number.Complex) {
          return new JsonNumber(((escm.type.number.Complex) datum).realPart().doubleValue());
        }
        return new JsonNumber(((escm.type.number.Real) datum).doubleValue());
      } else if (datum instanceof escm.type.bool.Boolean) {
        return JsonBoolean.valueOf(datum == escm.type.bool.Boolean.TRUE);
      } else if (datum == Nil.VALUE) {
        return JsonNull.VALUE;
      } else {
        throw new Exceptionf("'(data->json <json-compliant-datum> <optional-tab-width>) invalid non-json datum: %s",
            datum.profile());
      }
    }

    static int getTabWidth(ArrayList<Datum> parameters) throws Exception {
      Datum tabWidthDatum = parameters.get(1);
      if (!(tabWidthDatum instanceof escm.type.number.Real)) {
        throw new Exceptionf("'(data->json <json-compliant-datum> <optional-tab-width>) invalid tab width: %s",
            Exceptionf.profileArgs(parameters));
      }
      escm.type.number.Real real = (escm.type.number.Real) tabWidthDatum;
      if (real.isInteger() == false) {
        throw new Exceptionf("'(data->json <json-compliant-datum> <optional-tab-width>) invalid tab width: %s",
            Exceptionf.profileArgs(parameters));
      }
      int intValue = ((escm.type.number.Real) tabWidthDatum).intValue();
      if (intValue < 0) {
        throw new Exceptionf("'(data->json <json-compliant-datum> <optional-tab-width>) invalid tab width: %s",
            Exceptionf.profileArgs(parameters));
      }
      return intValue;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if (n == 0 || n > 2)
        throw new Exceptionf("'(data->json <json-compliant-datum> <optional-tab-width>) invalid arg signature: %s",
            Exceptionf.profileArgs(parameters));
      Datum datum = parameters.get(0);
      int tabWidth = n == 1 ? 0 : getTabWidth(parameters);
      JsonDatum jsonDatum = convertEscmDatumToJsonDatum(datum);
      return new escm.type.String(jsonDatum.toString(tabWidth));
    }
  }

  public static class IsJsonDatum extends Primitive {
    public java.lang.String escmName() {
      return "json-datum?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("json-datum?"), new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:JSON\nDetermine whether <obj> is a JSON-compliant EScheme datum. JSON-compliant data\ninclude booleans, nils, numbers, and strings, as well as vectors and hashmaps\nthereof (so long as the hashmap keys are all strings).";
    }

    private static boolean escmDatumIsJsonDatum(Datum datum) throws Exception {
      if (datum instanceof escm.type.Vector) {
        escm.type.Vector vector = (escm.type.Vector) datum;
        for (int i = 0; i < vector.size(); i++) {
          if (escmDatumIsJsonDatum(vector.get(i)) == false) {
            return false;
          }
        }
        return true;
      } else if (datum instanceof escm.type.Hashmap) {
        escm.type.Hashmap hashmap = (escm.type.Hashmap) datum;
        Datum keys = hashmap.keys();
        while (keys instanceof Pair) {
          Pair p = (Pair) keys;
          Datum keyDatum = p.car();
          if (!(keyDatum instanceof escm.type.String)) {
            return false;
          }
          if (escmDatumIsJsonDatum(hashmap.get(keyDatum)) == false) {
            return false;
          }
          keys = p.cdr();
        }
        return true;
      } else if (datum instanceof escm.type.String) {
        return true;
      } else if (datum instanceof escm.type.number.Number) {
        return true;
      } else if (datum instanceof escm.type.bool.Boolean) {
        return true;
      } else if (datum == Nil.VALUE) {
        return true;
      } else {
        return false;
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if (parameters.size() != 1)
        throw new Exceptionf("'(json-datum? <obj>) invalid arg signature: %s",
            Exceptionf.profileArgs(parameters));
      return escm.type.bool.Boolean.valueOf(escmDatumIsJsonDatum(parameters.get(0)));
    }
  }

  public static class IsJsonString extends Primitive {
    public java.lang.String escmName() {
      return "json-string?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("json-string?"), new Symbol("<string>"));
    }

    public String docstring() {
      return "@help:Procedures:JSON\nDetermine whether <string> is a valid JSON string.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if (parameters.size() != 1)
        throw new Exceptionf("'(json-string? <string>) invalid arg signature: %s",
            Exceptionf.profileArgs(parameters));
      Datum arg = parameters.get(0);
      if (!(arg instanceof escm.type.String))
        throw new Exceptionf("'(json-string? <string>) arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      try {
        JsonDatum.parse(((escm.type.String) arg).value());
        return escm.type.bool.Boolean.TRUE;
      } catch (Exception e) {
        return escm.type.bool.Boolean.FALSE;
      }
    }
  }
}
