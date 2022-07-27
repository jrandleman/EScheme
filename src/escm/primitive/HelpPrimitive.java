// Author: Jordan Randleman - escm.primitive.HelpPrimitive
// Purpose:
//    Java primitive to help explain language features.
//
//      => NOTE: <help> only prints to STDOUT & accepts from STDIN
//         -> eg NOT (current-output-port) & (current-input-port)
//

package escm.primitive;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.nio.file.Files;
import java.io.File;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import escm.type.Datum;
import escm.type.Void;
import escm.util.Exceptionf;
import escm.util.Pair;
import escm.util.GetClosestStringMatches;
import escm.util.json.JsonDatum;
import escm.util.json.JsonArray;
import escm.util.json.JsonBoolean;
import escm.util.json.JsonNull;
import escm.util.json.JsonNumber;
import escm.util.json.JsonObject;
import escm.util.json.JsonString;
import escm.vm.type.Primitive;
import escm.vm.runtime.installerGenerated.EscmPath;

public class HelpPrimitive {
  public static class Help implements Primitive {
    //////////////////////////////////////////////////////////////////////////
    // Constants
    private static final int MAXIMUM_SUGGESTED_SEARCH_ALTERNATIVES = 25;

    private static final int HELP_MENU_COLUMNS = 5;

    private static final File dbDirectory = new File(EscmPath.VALUE + File.separator + "src"       
                                                                    + File.separator + "escm" 
                                                                    + File.separator + "primitive" 
                                                                    + File.separator + "HelpPrimitive_util"
                                                                    + File.separator + "HelpPrimitive_data");


    //////////////////////////////////////////////////////////////////////////
    // Help Database Caching
    private static JsonDatum db = null;

    private static final ArrayList<String> helpEntryNames = new ArrayList<String>();


    //////////////////////////////////////////////////////////////////////////
    // Terminate Help Exception
    private static class TerminateHelpMenuException extends Exception {
      public TerminateHelpMenuException() {
        super("Terminating the help menu.");
      }
    }


    //////////////////////////////////////////////////////////////////////////
    // "help" DB Caching
    private static synchronized boolean objectIsAHelpEntry(HashMap<String,JsonDatum> obj) {
      return obj.containsKey("name") && obj.containsKey("description");
    }


    private static synchronized void registerAllArrayHelpEntries(ArrayList<JsonDatum> arr) {
      for(JsonDatum d : arr) {
        if(!(d instanceof JsonObject)) continue;
        HashMap<String,JsonDatum> items = ((JsonObject)d).value();
        if(objectIsAHelpEntry(items)) {
          JsonDatum name = items.get("name");
          if(name != null && name instanceof JsonString) {
            helpEntryNames.add(((JsonString)name).value());
          }
        }
      }
    }

    
    private static synchronized JsonObject accumulateDirectoryEntries(File dir) throws Exception {
      HashMap<String,JsonDatum> dbObject = new HashMap<String,JsonDatum>();
      for(File entry : dir.listFiles()) {
        // Accumulate files
        String fileName = entry.getName();
        if(entry.isFile()) {
          if(fileName.endsWith(".json")) {
            String helpEntryType = fileName.substring(0,fileName.lastIndexOf(".json"));
            JsonDatum parsed = JsonDatum.parse(Files.readString(entry.toPath()));
            dbObject.put(helpEntryType,parsed);
            if(parsed instanceof JsonArray) {
              registerAllArrayHelpEntries(((JsonArray)parsed).value());
            }
          }
        // Recurse through directories
        } else {
          dbObject.put(fileName,accumulateDirectoryEntries(entry));
        }
      }
      return new JsonObject(dbObject);
    }


    private static synchronized void cacheHelpDB() throws Exception {
      if(db != null) return;
      db = accumulateDirectoryEntries(dbDirectory);
    }
    

    //////////////////////////////////////////////////////////////////////////
    // Alternative Query Suggestions
    private static synchronized void suggestPossiblyIntendedQueries(String query, boolean looping) throws Exception {
      ArrayList<String> potentialMatches = GetClosestStringMatches.run(query,helpEntryNames,MAXIMUM_SUGGESTED_SEARCH_ALTERNATIVES);
      System.out.println("\nNo matches found! Did you mean:");
      for(int i = 0, n = potentialMatches.size(); i < n; ++i) {
        System.out.printf("  %2d) %s\n", i+1, potentialMatches.get(i));
      }
      System.out.print("\nType in a new query, or :quit to quit:");
      prompt(looping);
    }
    

    //////////////////////////////////////////////////////////////////////////
    // Interactive DB traversal
    private static synchronized JsonDatum parseArrayForInteractiveQuery(ArrayList<String> queryTypes, ArrayList<JsonDatum> arr, String query) {
      for(JsonDatum item : arr) {
        if(!(item instanceof JsonObject)) continue;
        HashMap<String,JsonDatum> entry = ((JsonObject)item).value();
        // Check for name match
        JsonDatum name = entry.get("name");
        if(name != null && name instanceof JsonString && ((JsonString)name).value().equals(query)) {
          return item;
        }
        // Check for alias match
        JsonDatum aliases = entry.get("aliases");
        if(aliases != null && aliases instanceof JsonArray) {
          for(JsonDatum alias : ((JsonArray)aliases).value()) {
            if(alias instanceof JsonString && ((JsonString)alias).value().equals(query)) {
              return item;
            }
          }
        }
      }
      return null;
    }


    private static synchronized JsonDatum parseObjectForInteractiveQuery(ArrayList<String> queryTypes, HashMap<String,JsonDatum> obj, String query) {
      for(Map.Entry<String,JsonDatum> e : obj.entrySet()) {
        JsonDatum val = e.getValue();
        String name = e.getKey(); 
        if(val instanceof JsonObject) {
          if(name.equals(query)) return val;
          queryTypes.add(name);
          JsonDatum result = parseObjectForInteractiveQuery(queryTypes,((JsonObject)val).value(),query);
          if(result != null) return result;
          queryTypes.remove(queryTypes.size()-1);
        } else if(val instanceof JsonArray) {
          if(name.equals(query)) return val;
          queryTypes.add(name);
          JsonDatum result = parseArrayForInteractiveQuery(queryTypes,((JsonArray)val).value(),query);
          if(result != null) return result;
          queryTypes.remove(queryTypes.size()-1);
        }
      }
      return null;
    }


    private static synchronized Pair<ArrayList<String>,JsonDatum> getPromptInputHelpEntry(String input) {
      ArrayList<String> queryTypes = new ArrayList<String>();
      JsonDatum result = parseObjectForInteractiveQuery(queryTypes,((JsonObject)db).value(),input);
      if(result == null) return null;
      return new Pair<ArrayList<String>,JsonDatum>(queryTypes,result);
    }


    private static synchronized void prompt(boolean looping) throws Exception {
      while(true) {
        System.out.print("\nhelp> ");
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String input = br.readLine();
        if(input != null) input = input.trim();
        if(input == null || input.equals(":quit")) {
          if(input == null) System.out.print('\n');
          throw new TerminateHelpMenuException();
        }
        Pair<ArrayList<String>,JsonDatum> helpEntry = getPromptInputHelpEntry(input);
        if(helpEntry != null && helpEntry.second instanceof JsonArray) {
          printHelpOptions((JsonArray)helpEntry.second);
          throw new TerminateHelpMenuException();
        } else if(helpEntry != null && helpEntry.second instanceof JsonObject) {
          HashMap<String,JsonDatum> entries = ((JsonObject)helpEntry.second).value();
          if(objectIsAHelpEntry(entries)) {
            printQueryDetails(helpEntry.first,entries);
            if(looping == false) throw new TerminateHelpMenuException();
          } else {
            printHelpOptions((JsonObject)helpEntry.second);
            throw new TerminateHelpMenuException();
          }
        } else {
          suggestPossiblyIntendedQueries(input,looping);
          if(looping == false) throw new TerminateHelpMenuException();
        }
      }
    }


    private static synchronized void showOptionsMenu(ArrayList<String> options) throws Exception {
      // Get the lengths of each menu column
      int[] col_lengths = new int[HELP_MENU_COLUMNS];
      int totalOptions = options.size();
      for(int col = 0; col < HELP_MENU_COLUMNS; ++col) {
        for(int i = col; i < totalOptions; i += HELP_MENU_COLUMNS) {
          int entryLength = options.get(i).length();
          if(col_lengths[col] < entryLength) {
            col_lengths[col] = entryLength;
          }
        }
      }
      // Print the menu
      if(totalOptions > 0) System.out.print("\n  ");
      for(int i = 0; i < totalOptions; ++i) {
        String option = options.get(i);
        StringBuilder sb = new StringBuilder();
        sb.append(option);
        sb.append("  ");
        for(int space = 0, n = col_lengths[i%HELP_MENU_COLUMNS]-option.length(); space < n; ++space) {
          sb.append(' ');
        }
        System.out.print(sb.toString());
        if((i+1)%HELP_MENU_COLUMNS == 0) {
          System.out.print("\n  ");
        } else if(i+1 == totalOptions) {
          System.out.print('\n');
        }
      }
      // Prompt for an entry
      prompt(true);
    }


    private static synchronized void printHelpOptions(JsonObject obj) throws Exception {
      ArrayList<String> options = new ArrayList<String>();
      for(Map.Entry<String,JsonDatum> e : obj.value().entrySet()) {
        options.add(e.getKey());
      }
      showOptionsMenu(options);
    }


    private static synchronized void printHelpOptions(JsonArray arr) throws Exception {
      ArrayList<String> options = new ArrayList<String>();
      for(JsonDatum item : arr.value()) {
        if(item instanceof JsonObject) {
          JsonDatum name = ((JsonObject)item).value().get("name");
          if(name != null && name instanceof JsonString) {
            options.add(((JsonString)name).value());
          }
        }
      }
      showOptionsMenu(options);
    }


    private static synchronized void launchInteractiveMenu() throws Exception {
      System.out.print("\n===============================================================================\n");
      System.out.print("Welcome to EScheme's help menu!\n\nType :quit to quit, or any of the below sections for their help entries:\n");
      printHelpOptions((JsonObject)db);
    }


    //////////////////////////////////////////////////////////////////////////
    // Specific query traversal
    private static synchronized void printQueryDetails(ArrayList<String> queryTypes, HashMap<String,JsonDatum> entry) {
      StringBuilder sb = new StringBuilder("\n===============================================================================\n");
      JsonDatum nameDatum = entry.get("name");
      JsonDatum sigsDatum = entry.get("signatures");
      JsonDatum descriptionDatum = entry.get("description");
      JsonDatum exampleDatum = entry.get("example");
      // Print name
      if(nameDatum != null && nameDatum instanceof JsonString) {
        String name = ((JsonString)nameDatum).value();
        sb.append("Name: ");
        sb.append(name);
        sb.append('\n');
        for(int i = 0, n = name.length()+6; i < n; ++i) sb.append('*');
        sb.append("\n\n");
      }
      // Print classification
      if(queryTypes.size() > 0) {
        sb.append("Classification: ");
        for(int i = 0, n = queryTypes.size(); i < n; ++i) {
          sb.append(queryTypes.get(i));
          if(i+1 < n) sb.append(" > ");
        }
        sb.append("\n\n");
      }
      // Print signatures
      if(sigsDatum != null && sigsDatum instanceof JsonArray) {
        ArrayList<JsonDatum> sigs = ((JsonArray)sigsDatum).value();
        if(sigs.size() > 0) {
          sb.append("Signatures:\n");
          for(JsonDatum sig : sigs) {
            if(sig instanceof JsonString) {
              sb.append("  ");
              sb.append(((JsonString)sig).value());
              sb.append('\n');
            }
          }
          sb.append('\n');
        }
      }
      // Print description
      if(descriptionDatum != null && descriptionDatum instanceof JsonString) {
        String str = ((JsonString)descriptionDatum).value();
        if(str.length() > 0) {
          sb.append("Description:\n  ");
          sb.append(str.replaceAll("\n","\n  "));
          if(exampleDatum != null && exampleDatum instanceof JsonString) {
            sb.append("\n\n");
          } else {
            sb.append("\n===============================================================================\n");
          }
        }
      }
      // Print Example(s)
      if(exampleDatum != null && exampleDatum instanceof JsonString) {
        String str = ((JsonString)exampleDatum).value();
        if(str.length() > 0) {
          sb.append("Example(s):\n  ");
          sb.append(str.replaceAll("\n","\n  "));
          sb.append("\n===============================================================================\n");
        }
      }
      System.out.print(sb.toString());
    }


    private static synchronized boolean parseArrayForQuery(ArrayList<String> queryTypes, ArrayList<JsonDatum> arr, String query) {
      for(JsonDatum item : arr) {
        if(!(item instanceof JsonObject)) continue;
        HashMap<String,JsonDatum> entry = ((JsonObject)item).value();
        // Check for name match
        JsonDatum name = entry.get("name");
        if(name != null && name instanceof JsonString && ((JsonString)name).value().equals(query)) {
          printQueryDetails(queryTypes,entry);
          return true;
        }
        // Check for alias match
        JsonDatum aliases = entry.get("aliases");
        if(aliases != null && aliases instanceof JsonArray) {
          for(JsonDatum alias : ((JsonArray)aliases).value()) {
            if(alias instanceof JsonString && ((JsonString)alias).value().equals(query)) {
              printQueryDetails(queryTypes,entry);
              return true;
            }
          }
        }
      }
      return false;
    }


    private static synchronized boolean parseObjectForQuery(ArrayList<String> queryTypes, HashMap<String,JsonDatum> obj, String query) {
      for(Map.Entry<String,JsonDatum> e : obj.entrySet()) {
        JsonDatum val = e.getValue();
        if(val instanceof JsonObject) {
          queryTypes.add(e.getKey());
          if(parseObjectForQuery(queryTypes,((JsonObject)val).value(),query)) return true;
          queryTypes.remove(queryTypes.size()-1);
        } else if(val instanceof JsonArray) {
          queryTypes.add(e.getKey());
          if(parseArrayForQuery(queryTypes,((JsonArray)val).value(),query)) return true;
          queryTypes.remove(queryTypes.size()-1);
        }
      }
      return false;
    }


    private static synchronized void executeQuery(String query) throws Exception {
      ArrayList<String> queryTypes = new ArrayList<String>();
      if(parseObjectForQuery(queryTypes,((JsonObject)db).value(),query) == false) {
        suggestPossiblyIntendedQueries(query,false);
        prompt(false);
      }
    }


    //////////////////////////////////////////////////////////////////////////
    // Describe a given object
    private static synchronized void describeObject(Datum query) {
      System.out.println("\n  Datum "+query.profile()+'\n');
    }


    //////////////////////////////////////////////////////////////////////////
    // Main dispatch
    public java.lang.String escmName() {
      return "help";
    }


    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      cacheHelpDB(); // only caches upon first invocation!
      if(parameters.size() > 1)
        throw new Exceptionf("'(help <optional-query-symbol-or-obj>) received more than 1 arg: %s", Exceptionf.profileArgs(parameters));
      try {
        if(parameters.size() == 0) {
          launchInteractiveMenu();
        } else {
          Datum query = parameters.get(0);
          if(query instanceof escm.type.Symbol) {
            executeQuery(((escm.type.Symbol)query).value());
          } else {
            describeObject(query);
          }
        }
      } catch(TerminateHelpMenuException e) {
        // Ignore this exception, thrown when user enters :quit or ^D (EOF)
      }
      return Void.VALUE;
    }
  }
}