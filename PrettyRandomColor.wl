(* ::Package:: *)

Package["PrettyRandomColor`"]

PackageExport["PrettyRandomColor"]
PackageExport["Seed"]
PackageExport["Luminosity"]
PackageExport["ColorCount"]



Seed::usage="An option for generating repeatable colors.";
ColorCount::usage="An option for specify the number of colors to be generated.";
Luminosity::usage="An option for specify the luminosity of colors to be generated.";

PrettyRandomColor::invalidhex = "`1` is not a valid Hex.";

Options[PrettyRandomColor] = {
	Hue -> Automatic,
	Seed -> None, 
	ColorCount -> 1, 
	Luminosity -> Automatic,
	Opacity -> 1
};

PrettyRandomColor[OptionsPattern[]] := Module[
	{hueName, seed, seeds, count, luminosity, opacity, H, S, B, rgb, result},
	hueName = OptionValue[Hue];
	seed = Switch[#,
		_Integer, #,
		_String, Total @ ToCharacterCode[#],
		_, None
	]& [OptionValue[Seed]];
	count = OptionValue[ColorCount];
	luminosity = OptionValue[Luminosity];
	seeds = If[seed === None, ConstantArray[None, count],
					Range[seed + 1, seed + count]
			];
	opacity = OptionValue[Opacity];
	
	H = pickHue[If[hueName === Automatic, Monochrome, hueName], count, seeds];
	S = MapThread[pickSaturation[#1, hueName, luminosity, #2]&, {H, seeds}];
	B = MapThread[pickBrightness[#1, #2, luminosity, #3]&, {H, S, seeds}];
	
	rgb = (HSVToRGB /@ Transpose[{H ,S, B}]) / 255.;
	
	result = If[NumberQ[opacity] && 0 <= opacity <= 1,
		RGBColor[Sequence @@ #, Clip[opacity, {0, 1}]]& /@ rgb, 
		RGBColor /@ rgb
	];
	If[count == 1, result[[1]], result]
];


PackageScope["pickHue"]

pickHue[hueName_, count_, seeds_] := Module[{hueRange, hue, hues},
	If[count >= 2,
		hueRange = parseHueRange[hueName];
		hues = MapThread[
			randomWithin[#1, #2]&,
			{
				Partition[Subdivide[hueRange[[1]], hueRange[[2]], count], 2, 1],
				seeds
			}
		];
		hues = If[# < 0, # + 360, #]& /@ hues;
	,
		hueRange = parseHueRangeStrict[hueName];
		hue = randomWithin[hueRange, seeds[[1]]];
		hue = If[# < 0, # + 360, #]&[hue];
		hues = {hue};
	];
	hues
];


pickSaturation[hue_, hueName_, luminosity_, seed_] := Module[{smin, smax},
	If[hueName === Monochrome, Return[0]];
	If[ToLowerCase @ luminosity === "random", 
		Return[randomWithin[{0, 100}, seed]];
	];
	{smin, smax} = getSaturationRangeByValue @ hue;
	If[StringQ[luminosity],
		Switch[ToLowerCase @ luminosity, 
			"bright", smin = 55,
			"dark", smin = smax - 10,
			"light", smax = 55
		];
	];
	randomWithin[{smin, smax}, seed]

];

getMinimumBrightness[hue_, s_] := Module[{lb, pos, m, b},
	lb = getLowerBounds @ getHueNameByValue @ hue;
	pos = FirstPosition[lb, {ss_, vv_} /; ss >= s];
	If[!MissingQ[pos] && pos[[1]] >= 2,
		pos = pos[[1]];
		m = (lb[[pos, 2]] - lb[[pos - 1, 2]]) / (lb[[pos, 1]] - lb[[pos - 1, 1]]);
		b = lb[[pos - 1, 2]] - m * lb[[pos - 1, 1]];
		N[m * s + b]
	,
		0
	]
];

pickBrightness[hue_, s_, luminosity_, seed_] := Module[{bmin, bmax},
	bmin = getMinimumBrightness[hue, s];
	bmax = 100;
	If[StringQ[luminosity],
		Switch[ToLowerCase @ luminosity, 
			"dark", bmax = bmin + 20,
			"light", bmin = (bmax + bmin) / 2,
			"random", bmin = 0
		];
	];
	randomWithin[{bmin, bmax}, seed]
];


parseHueRange[hue_]:=
	If[IntegerQ[hue],
		If[0 <= hue <= 360, 
		getHueRange @ getHueNameByValue @ hue, {0, 360}],
		If[isHueName[hue],
			getHueRange[hue],
			If[isValidHexString[hue],
				Quiet @ getHueRange @ getHueNameByValue @ (HexToHSB[hue][[1]]),
				{0, 360}
			]
		]
	];

parseHueRangeStrict[hue_]:=
	If[IntegerQ[hue],
		If[0 <= hue <= 360, 
		{hue, hue}, {0, 360}],
		If[isHueName[hue],
			getHueRange[hue],
			If[isValidHexString[hue],
				{#, #} & [HexToHSB[hue][[1]]],
				{0, 360}
			]
		]
	];

randomWithin[range_, seed_ : None] := Module[{r},
	If[seed === None,
		r = Mod[GoldenRatio + RandomReal[] - 1, 1];
		Floor[range[[1]] + r * (range[[2]] + 1 - range[[1]])]
	,
		r = Mod[(seed * 9301 + 49297), 233280] / 233280.;
		Floor[range[[1]] + r * (range[[2]] - range[[1]])]
	]
];


RGBToHex[rgb_] := 
	"#" ~~ StringJoin[StringPadLeft[IntegerString[#, 16], 2, "0"]& /@ rgb];

HexToRGB[str_] := Module[{hex},
	hex = StringReplace[str, "#" -> ""];
	If[StringLength[hex] == 3,
		hex = StringReplace[hex, x_ :> x ~~ x]
	];
	If[StringLength[hex] != 6,
		Message[PrettyRandomColor::invalidhex, str];
		{255, 255, 255}
	,
	FromDigits[StringTake[hex, #], 16]& /@ Partition[Range[1, 6], 2]
	]
];

HSVToRGB[{hh_, ss_, vv_}] := Module[
	{h, s, v, hi, f, p, q, t, r, g, b},
	
	h = If[hh == 0, 1, If[hh == 360, 359, hh]];
	h = h / 360.;
	s = ss / 100.;
	v = vv / 100.;
	
	hi = Floor[6 h];
	f = 6 h - hi;
	p = v * (1 - s);
	q = v * (1 - f * s);
	t = v * (1 - (1 - f) * s);
	
	{r, g, b} = Switch[hi,
		0, {v, t, p},
		1, {q, v, p},
		2, {p, v, t},
		3, {p, q, v},
		4, {t, p, v},
		5, {v, p, q}
	];
	
	Floor /@ ({r, g, b} * 255)
];

HSVToHex[hsv_] := RGBToHex @ HSVToRGB @ hsv;

HSVToHSL[{hh_, ss_, vv_}]:=Module[{h, s, v, k},
	h = hh;
	s = ss / 100.;
	v = vv / 100.;
	k = (2 - s) * v;

	{h, Round[s * v / If[k < 1, k, 2 - k] * 10000] / 100., k / 2 * 100}
];

RGBToHSB[{rr_, gg_, bb_}]:=Module[{r, g, b, cmax, delta, saturation},
	{r, g, b} = {rr, gg, bb} / 255.;
	cmax = Max[{r, g, b}];
	delta = cmax - Min[{r, g, b}];
	saturation = If[cmax == 0, 0, delta / cmax];
	{
		If[delta == 0, 0,
			Switch[cmax,
				r, 60 * Mod[(g - b) / delta, 6],
				g, 60 * (b - r) / delta + 2,
				b, 60 * (r - g) / delta + 4
			]
		],
	saturation, cmax}
];

HexToHSB[str_] := RGBToHSB @ HexToRGB @ str;

isValidHexString[str_String] :=
	StringMatchQ[str, 
		RegularExpression["(?i)^#?([0-9A-F]{3}|[0-9A-F]{6})$"]
	];
isValidHexString[__] = False;


PrettyRandomColor::nohuename = "Unsupported Hue name specification \"`1`\". Monochrome is used instead.";
PrettyRandomColor::nohuevalue = "No hue name with value `1` found. Monochrome is used instead.";

isHueName[hueName_] := MemberQ[Keys[colorBounds], hueName];

checkHueName[hueName_] := 
	If[!isHueName[hueName],
		Message[PrettyRandomColor::nohuename, hueName]; Monochrome,
		hueName
	];

getHueRange[hueName_] := With[{name = checkHueName@hueName},
	If[name === Monochrome,
		{0, 360},
		colorBounds[name]["HueRange"]
	]
];


getLowerBounds[hueName_] :=
	colorBounds[checkHueName@hueName]["LowerBounds"];



getSaturationRange[hueName_] :=
	colorBounds[checkHueName@hueName]["LowerBounds"][[{1, -1}, 1]];
	

getBrightnessRange[hueName_] :=
	colorBounds[checkHueName@hueName]["LowerBounds"][[{-1, 1}, 2]];
	

getHueNameByValue[hue_] := Module[{h, idx},
	h = If[hue >= 334 && hue <= 360, hue - 360, hue];
	idx = FirstPosition[
		Normal @ colorBounds,
		HoldPattern[_ -> <|"HueRange" -> {min_, max_}, __|>] /; min - 0.5 <= h < max + 0.5
	];
	If[!MissingQ[idx], 
		Keys[colorBounds][[First @ idx]],
		Message[PrettyRandomColor::nohuevalue, hue]; Monochrome
	]
];


getSaturationRangeByValue[hue_] := 
	getSaturationRange @ getHueNameByValue @ hue;


PackageExport["Monochrome"]

Monochrome::usage = "Symbol representing monochrome color.";

colorBounds = <|
	Monochrome -> <|
		"HueRange" -> None, 
		"LowerBounds" -> {{0,0},{100,0}}
	|>,
	Red -> <|
		"HueRange" -> {-26,18}, 
		"LowerBounds" -> {{20,100},{30,92},{40,89},{50,85},{60,78},{70,70},{80,60},{90,55},{100,50}}
	|>,
	Orange -> <|
		"HueRange" -> {19,46}, 
		"LowerBounds" -> {{20,100},{30,93},{40,88},{50,86},{60,85},{70,70},{100,70}}
	|>,
	Yellow -> <|
		"HueRange" -> {47,62}, 
		"LowerBounds" -> {{25,100},{40,94},{50,89},{60,86},{70,84},{80,82},{90,80},{100,75}}
	|>,
	Green -> <|
		"HueRange" -> {63,178}, 
		"LowerBounds" -> {{30,100},{40,90},{50,85},{60,81},{70,74},{80,64},{90,50},{100,40}}
	|>,
	Blue -> <|
		"HueRange" -> {178,257}, 
		"LowerBounds" -> {{20,100},{30,86},{40,80},{50,74},{60,60},{70,52},{80,44},{90,39},{100,35}}
	|>,
	Purple -> <|
		"HueRange" -> {258,282}, 
		"LowerBounds" -> {{20,100},{30,87},{40,79},{50,70},{60,65},{70,59},{80,52},{90,45},{100,42}}
	|>,
	Pink -> <|
		"HueRange" -> {283,334}, 
		"LowerBounds" -> {{20,100},{30,90},{40,86},{60,84},{80,80},{90,75},{100,73}}
	|>
|>;
