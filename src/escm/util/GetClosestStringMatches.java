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
  private static int getLevenshteinDistance(String X, String Y) {
    int m = X.length();
    int n = Y.length();
    int[][] T = new int[m+1][n+1];
    for(int i = 1; i <= m; i++) {
      T[i][0] = i;
    }
    for(int j = 1; j <= n; j++) {
      T[0][j] = j;
    }
    for(int i = 1; i <= m; i++) {
      for(int j = 1; j <= n; j++) {
        int cost = X.charAt(i-1) == Y.charAt(j-1) ? 0 : 1;
        T[i][j] = Integer.min(Integer.min(T[i-1][j]+1,T[i][j-1]+1),T[i-1][j-1]+cost);
      }
    }
    return T[m][n];
  }
 

  public static double levenshteinSimilarity(String X, String Y) {
    double maxLength = Double.max(X.length(),Y.length());
    if(maxLength > 0) {
      return (maxLength-getLevenshteinDistance(X,Y))/maxLength;
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
    String lowerStr = str.toLowerCase();
    for(String alt : alts) {
      sortedSimilarities.add(new Pair<String,Double>(alt,levenshteinSimilarity(lowerStr,alt.toLowerCase())));
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