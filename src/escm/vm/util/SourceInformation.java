// Author: Jordan Randleman - escm.vm.util.SourceInformation
// Purpose:
//    Hold source file/line/column information, as provided by the reader,
//    to be held by the read escm.type.Symbol

package escm.vm.util;

public class SourceInformation {
  ////////////////////////////////////////////////////////////////////////////
  // Private Fields
  private String fileName = null;
  private long lineNumber = 1;
  private long columnNumber = 1;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public SourceInformation(String fileName, long lineNumber, long columnNumber) {
    this.fileName = fileName;
    this.lineNumber = lineNumber;
    this.columnNumber = columnNumber;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Cloning
  public synchronized SourceInformation clone() {
    return new SourceInformation(fileName,lineNumber,columnNumber);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public synchronized String toString() {
    StringBuilder sb = new StringBuilder("file=\"");
    sb.append(fileName);
    sb.append("\", line=");
    sb.append(lineNumber);
    sb.append(", column=");
    sb.append(columnNumber);
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Getters
  public synchronized java.lang.String fileName() {
    return fileName;
  }

  public synchronized long lineNumber() {
    return lineNumber;
  }

  public synchronized long columnNumber() {
    return columnNumber;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Position Updates (used by <escm.vm.Reader>)
  public synchronized void updatePosition(int c) {
    if(c <= 0) return;
    if(c == '\n') {
      ++lineNumber;
       columnNumber = 1; 
    } else {
      ++columnNumber;
    }
  }

  public synchronized void updatePosition(CharSequence s) {
    if(s == null) return;
    for(int i = 0, n = s.length(); i < n; ++i) {
      updatePosition(s.charAt(i));
    }
  }

  public synchronized void updatePosition(char[] cs) {
    if(cs == null) return;
    for(int i = 0; i < cs.length && cs[i] != 0; ++i) {
      updatePosition(cs[i]);
    }
  }
}