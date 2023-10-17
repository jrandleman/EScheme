// Author: Jordan Randleman - escm.primitive.lib.help.fs.HelpNode
// Purpose:
//    Abstract base class representing a node in our fs (file system) tree!

package escm.primitive.lib.help.fs;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.nio.file.Files;
import java.io.File;
import escm.type.Datum;
import escm.util.json.JsonDatum;
import escm.util.json.JsonArray;
import escm.util.json.JsonObject;
import escm.util.json.JsonString;
import escm.vm.util.Environment;
import escm.primitive.MetaPrimitives;
import escm.vm.runtime.installerGenerated.EscmPath;

public abstract class HelpNode {
  ////////////////////////////////////////////////////////////////////////////
  // Static Invariants for the Help FS
  static final String UNCATEGORIZED_VARIABLES_FOLDER_NAME = "Misc";
  static final String HOME_DIRECTORY_FOLDER_NAME = "~";
  static final String RESERVED_PREFIX_1 = new String(new char[]{(char)101,(char)115,(char)99,(char)109,(char)45});
  static final String RESERVED_PREFIX_2 = new String(new char[]{(char)116,(char)97,(char)115,(char)110,(char)105,(char)109});


  ////////////////////////////////////////////////////////////////////////////
  // Help FS Environment Variable Parsing
  private static void registerEnvironmentVariables(Environment env, FolderNode home) {
    ConcurrentHashMap<String,Datum> bindings = env.bindings();
    for(ConcurrentHashMap.Entry<String,Datum> item : bindings.entrySet()) {
      Datum obj = item.getValue();
      String[] dir = MetaPrimitives.DocStringExtractor.helpLocation(obj);
      if(dir != null) {
        home.addObject(dir,item.getKey(),obj);
      } else {
        home.addObject(new String[]{UNCATEGORIZED_VARIABLES_FOLDER_NAME},item.getKey(),obj);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Help FS Preset Help Topics JSON parsing
  private static final File dataDirectory = new File(EscmPath.VALUE + File.separator + "src"       
                                                                    + File.separator + "escm" 
                                                                    + File.separator + "primitive" 
                                                                    + File.separator + "lib"
                                                                    + File.separator + "help"
                                                                    + File.separator + "data");


  private static HashMap<String,String> parseFolderTopics(JsonDatum obj) {
    if(!(obj instanceof JsonArray)) return null;
    HashMap<String,String> topics = new HashMap<String,String>();
    ArrayList<JsonDatum> arr = ((JsonArray)obj).value();
    for(JsonDatum topic : arr) {
      if(topic instanceof JsonObject) {
        HashMap<String,JsonDatum> topicObj = ((JsonObject)topic).value();
        JsonDatum name = topicObj.get("name");
        JsonDatum description = topicObj.get("description");
        if(name != null && name instanceof JsonString && description != null && description instanceof JsonString) {
          topics.put(((JsonString)name).value(),((JsonString)description).value());
        }
      }
    }
    return topics;
  }


  private static HashMap<String,HashMap<String,String>> getPresetJsonTopicDirectories(File dir) {
    HashMap<String,HashMap<String,String>> folders = new HashMap<String,HashMap<String,String>>();
    for(File entry : dir.listFiles()) {
      // Accumulate files
      String fileName = entry.getName();
      if(entry.isFile()) {
        if(fileName.endsWith(".json")) {
          String helpEntryType = fileName.substring(0,fileName.lastIndexOf(".json"));
          try {
            JsonDatum json = JsonDatum.parse(Files.readString(entry.toPath()));
            HashMap<String,String> parsedTopics = parseFolderTopics(json);
            if(parsedTopics != null) folders.put(helpEntryType,parsedTopics);
          } catch(Exception e) {
            // ignore failed file read  
          }
        }
      }
    }
    return folders;
  }


  private static void registerPresetJsonTopics(FolderNode home) {
    HashMap<String,HashMap<String,String>> folders = getPresetJsonTopicDirectories(dataDirectory);
    for(HashMap.Entry<String,HashMap<String,String>> folder : folders.entrySet()) {
      String folderName = folder.getKey();
      HashMap<String,String> topics = folder.getValue();
      for(HashMap.Entry<String,String> topic : topics.entrySet()) {
        String topicName = topic.getKey();
        String topicDescription = topic.getValue();
        home.addTopic(new String[]{folderName},topicName,topicDescription);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // FS Creation from a Given Environment
  // Called to get the root the help directory
  public static FolderNode createHomeDirectory(Environment env) {
    FolderNode home = new FolderNode();
    registerEnvironmentVariables(env,home);
    registerPresetJsonTopics(home);
    return home;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Required FS Methods
  public abstract FolderNode getParent();
  public abstract String getPath();
  public abstract Datum toDatum();
  public abstract void print();
}