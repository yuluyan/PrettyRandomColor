(* ::Package:: *)

If[Not@OrderedQ[{11.0, 0}, {$VersionNumber, $ReleaseNumber}],
  Print["PrettyRandomColor requires Mathematica 11.0.0 or later."];
  Abort[]
]

Unprotect["PrettyRandomColor`*", "PrettyRandomColor`Private`*"];

SetAttributes[
  Evaluate @ Flatten[Names /@ {"PrettyRandomColor`*", "PrettyRandomColor`Private`*"}],
  {Protected, ReadProtected}
]
