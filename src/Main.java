// Author: Jordan Randleman - Main
// Purpose:
//    Wrapper around escm.vm.Main to simplify compilation and cmd-line invocation
public class Main {
  public static void main(String[] args) {
    escm.vm.Main.launchESchemeSession(args);
  }
}