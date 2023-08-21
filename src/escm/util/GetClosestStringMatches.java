// Author: Jordan Randleman - escm.util.GetClosestStringMatches
// Purpose:
//    Get an ArrayList of Strings that most closely match the given string
//    from a list of potential alternatives.

package escm.util;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.TreeSet;

public class GetClosestStringMatches {
  //////////////////////////////////////////////////////////////////////////
  // String Similarity Computation
  //   => Algorithm From: https://www.techiedelight.com/calculate-string-similarity-java/
  private static int getLevenshteinDistance(String x, String y) {
    int m = x.length();
    int n = y.length();
    int[][] t = new int[m+1][n+1];
    for(int i = 1; i <= m; i++) {
      t[i][0] = i;
    }
    for(int j = 1; j <= n; j++) {
      t[0][j] = j;
    }
    for(int i = 1; i <= m; i++) {
      for(int j = 1; j <= n; j++) {
        int cost = x.charAt(i-1) == y.charAt(j-1) ? 0 : 1;
        t[i][j] = Integer.min(Integer.min(t[i-1][j]+1,t[i][j-1]+1),t[i-1][j-1]+cost);
      }
    }
    return t[m][n];
  }
 

  public static double levenshteinSimilarity(String x, String y) {
    double maxLength = Double.max(x.length(),y.length());
    if(maxLength > 0) {
      return (maxLength-getLevenshteinDistance(x,y))/maxLength;
    }
    return 1.0;
  }


  //////////////////////////////////////////////////////////////////////////
  // Main Dispatch
  public static ArrayList<String> run(String str, ArrayList<String> alts, int maxMatches) {
    TreeSet<Pair<String,Double>> sortedSimilarities = new TreeSet<Pair<String,Double>>(new Comparator<Pair<String,Double>>() {
      public int compare(Pair<String,Double> p1, Pair<String,Double> p2) {
        if(p1.second < p2.second) return 1;
        if(p1.second == p2.second) return 0;
        return -1;
      }
    });
    for(String alt : alts) {
      sortedSimilarities.add(new Pair<String,Double>(alt,levenshteinSimilarity(str,alt)));
    }
    int count = 0;
    ArrayList<String> similarItems = new ArrayList<String>();
    for(Pair<String,Double> e : sortedSimilarities) {
      if(count >= maxMatches) return similarItems;
      similarItems.add(e.first);
      ++count;
    }
    return similarItems;
  }
}