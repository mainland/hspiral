#include <complex.h>
#include <stdint.h>
void hspiral_dft_32(const double X[restrict static 64],
                    double Y[restrict static 64])
{
    double t;
    double t1;
    double t2;
    double t3;
    double t4;
    double t5;
    double t6;
    double t7;
    double t8;
    double t9;
    double t10;
    double t11;
    double t12;
    double t13;
    double t14;
    double t15;
    double t16;
    double t17;
    double t18;
    double t19;
    double t20;
    double t21;
    double t22;
    double t23;
    double t24;
    double t25;
    double t26;
    double t27;
    double t28;
    double t29;
    double t30;
    double t31;
    double t32;
    double t33;
    double t34;
    double t35;
    double t36;
    double t37;
    double t38;
    double t39;
    double t40;
    double t41;
    double t42;
    double t43;
    double t44;
    double t45;
    double t46;
    double t47;
    double t48;
    double t49;
    double t50;
    double t51;
    double t52;
    double t53;
    double t54;
    double t55;
    double t56;
    double t57;
    double t58;
    double t59;
    double t60;
    double t61;
    double t62;
    double t63;
    double t64;
    double t65;
    double t66;
    double t67;
    double t68;
    double t69;
    double t70;
    double t71;
    double t72;
    double t73;
    double t74;
    double t75;
    double t76;
    double t77;
    double t78;
    double t79;
    double t80;
    double t81;
    double t82;
    double t83;
    double t84;
    double t85;
    double t86;
    double t87;
    double t88;
    double t89;
    double t90;
    double t91;
    double t92;
    double t93;
    double t94;
    double t95;
    double t96;
    double t97;
    double t98;
    double t99;
    double t100;
    double t101;
    double t102;
    double t103;
    double t104;
    double t105;
    double t106;
    double t107;
    double t108;
    double t109;
    double t110;
    double t111;
    double t112;
    double t113;
    double t114;
    double t115;
    double t116;
    double t117;
    double t118;
    double t119;
    double t120;
    double t121;
    double t122;
    double t123;
    double t124;
    double t125;
    double t126;
    double t127;
    double t128;
    double t129;
    double t130;
    double t131;
    double t132;
    double t133;
    double t134;
    double t135;
    double t136;
    double t137;
    double t138;
    double t139;
    double t140;
    double t141;
    double t142;
    double t143;
    double t144;
    double t145;
    double t146;
    double t147;
    double t148;
    double t149;
    double t150;
    double t151;
    double t152;
    double t153;
    double t154;
    double t155;
    double t156;
    double t157;
    double t158;
    double t159;
    double t160;
    double t161;
    double t162;
    double t163;
    double t164;
    double t165;
    double t166;
    double t167;
    double t168;
    double t169;
    double t170;
    double t171;
    double t172;
    double t173;
    double t174;
    double t175;
    double t176;
    double t177;
    double t178;
    double t179;
    double t180;
    double t181;
    double t182;
    double t183;
    double t184;
    double t185;
    double t186;
    double t187;
    double t188;
    double t189;
    double t190;
    double t191;
    double t192;
    double t193;
    double t194;
    double t195;
    double t196;
    double t197;
    double t198;
    double t199;
    double t200;
    double t201;
    double t202;
    double t203;
    double t204;
    double t205;
    double t206;
    double t207;
    double t208;
    double t209;
    double t210;
    double t211;
    double t212;
    double t213;
    double t214;
    double t215;
    double t216;
    double t217;
    double t218;
    double t219;
    double t220;
    double t221;
    double t222;
    double t223;
    double t224;
    double t225;
    double t226;
    double t227;
    double t228;
    double t229;
    double t230;
    double t231;
    double t232;
    double t233;
    double t234;
    double t235;
    double t236;
    double t237;
    double t238;
    double t239;
    double t240;
    double t241;
    double t242;
    double t243;
    double t244;
    double t245;
    double t246;
    double t247;
    double t248;
    double t249;
    double t250;
    double t251;
    double t252;
    double t253;
    double t254;
    double t255;
    double t256;
    double t257;
    double t258;
    double t259;
    double t260;
    double t261;
    double t262;
    double t263;
    double t264;
    double t265;
    double t266;
    double t267;
    double t268;
    double t269;
    double t270;
    double t271;
    double t272;
    double t273;
    double t274;
    double t275;
    double t276;
    double t277;
    double t278;
    double t279;
    double t280;
    double t281;
    double t282;
    double t283;
    double t284;
    double t285;
    double t286;
    double t287;
    double t288;
    double t289;
    double t290;
    double t291;
    double t292;
    double t293;
    double t294;
    double t295;
    double t296;
    double t297;
    double t298;
    double t299;
    double t300;
    double t301;
    double t302;
    double t303;
    double t304;
    double t305;
    double t306;
    double t307;
    double t308;
    double t309;
    double t310;
    double t311;
    double t312;
    double t313;
    double t314;
    double t315;
    double t316;
    double t317;
    double t318;
    double t319;
    double t320;
    double t321;
    double t322;
    double t323;
    double t324;
    double t325;
    double t326;
    double t327;
    double t328;
    double t329;
    double t330;
    double t331;
    double t332;
    double t333;
    double t334;
    double t335;
    double t336;
    double t337;
    double t338;
    double t339;
    double t340;
    double t341;
    double t342;
    double t343;
    double t344;
    double t345;
    double t346;
    double t347;
    double t348;
    double t349;
    double t350;
    double t351;
    double t352;
    double t353;
    double t354;
    double t355;
    double t356;
    double t357;
    double t358;
    double t359;
    double t360;
    double t361;
    double t362;
    double t363;
    double t364;
    double t365;
    double t366;
    double t367;
    double t368;
    double t369;
    double t370;
    double t371;
    double t372;
    double t373;
    double t374;
    double t375;
    double t376;
    double t377;
    double t378;
    double t379;
    double t380;
    double t381;
    double t382;
    double t383;
    double t384;
    double t385;
    double t386;
    double t387;
    double t388;
    double t389;
    double t390;
    double t391;
    double t392;
    double t393;
    double t394;
    double t395;
    double t396;
    double t397;
    double t398;
    double t399;
    double t400;
    double t401;
    double t402;
    double t403;
    double t404;
    double t405;
    double t406;
    double t407;
    double t408;
    double t409;
    double t410;
    double t411;
    double t412;
    double t413;
    double t414;
    double t415;
    double t416;
    double t417;
    double t418;
    double t419;
    double t420;
    double t421;
    double t422;
    double t423;
    double t424;
    double t425;
    double t426;
    double t427;
    double t428;
    double t429;
    double t430;
    double t431;
    double t432;
    double t433;
    double t434;
    double t435;
    double t436;
    double t437;
    double t438;
    double t439;
    double t440;
    double t441;
    double t442;
    double t443;
    double t444;
    double t445;
    double t446;
    double t447;
    double t448;
    double t449;
    double t450;
    double t451;
    double t452;
    double t453;
    double t454;
    double t455;
    double t456;
    double t457;
    double t458;
    double t459;
    double t460;
    double t461;
    double t462;
    double t463;
    double t464;
    double t465;
    double t466;
    double t467;
    double t468;
    double t469;
    double t470;
    double t471;
    double t472;
    double t473;
    double t474;
    double t475;
    double t476;
    double t477;
    double t478;
    double t479;
    double t480;
    double t481;
    double t482;
    double t483;
    double t484;
    double t485;
    double t486;
    double t487;
    double t488;
    double t489;
    double t490;
    double t491;
    double t492;
    double t493;
    double t494;
    double t495;
    double t496;
    double t497;
    double t498;
    double t499;
    double t500;
    double t501;
    double t502;
    double t503;
    double t504;
    double t505;
    double t506;
    double t507;
    double t508;
    double t509;
    double t510;
    double t511;
    
    t = X[0];
    t1 = X[1];
    t2 = X[4];
    t3 = X[5];
    t4 = X[8];
    t5 = X[9];
    t6 = X[12];
    t7 = X[13];
    t8 = X[16];
    t9 = X[17];
    t10 = X[20];
    t11 = X[21];
    t12 = X[24];
    t13 = X[25];
    t14 = X[28];
    t15 = X[29];
    t16 = X[32];
    t17 = X[33];
    t18 = X[36];
    t19 = X[37];
    t20 = X[40];
    t21 = X[41];
    t22 = X[44];
    t23 = X[45];
    t24 = X[48];
    t25 = X[49];
    t26 = X[52];
    t27 = X[53];
    t28 = X[56];
    t29 = X[57];
    t30 = X[60];
    t31 = X[61];
    t32 = X[2];
    t33 = X[3];
    t34 = X[10];
    t35 = X[11];
    t36 = X[18];
    t37 = X[19];
    t38 = X[26];
    t39 = X[27];
    t40 = X[34];
    t41 = X[35];
    t42 = X[42];
    t43 = X[43];
    t44 = X[50];
    t45 = X[51];
    t46 = X[58];
    t47 = X[59];
    t48 = X[6];
    t49 = X[7];
    t50 = X[14];
    t51 = X[15];
    t52 = X[22];
    t53 = X[23];
    t54 = X[30];
    t55 = X[31];
    t56 = X[38];
    t57 = X[39];
    t58 = X[46];
    t59 = X[47];
    t60 = X[54];
    t61 = X[55];
    t62 = X[62];
    t63 = X[63];
    t64 = t + t16;
    t65 = t1 + t17;
    t66 = t - t16;
    t67 = t1 - t17;
    t68 = t8 + t24;
    t69 = t9 + t25;
    t70 = t8 - t24;
    t71 = t9 - t25;
    t72 = t64 + t68;
    t73 = t65 + t69;
    t74 = t64 - t68;
    t75 = t65 - t69;
    t76 = t66 + t71;
    t77 = t67 - t70;
    t78 = t66 - t71;
    t79 = t67 + t70;
    t80 = t2 + t18;
    t81 = t3 + t19;
    t82 = t2 - t18;
    t83 = t3 - t19;
    t84 = t10 + t26;
    t85 = t11 + t27;
    t86 = t10 - t26;
    t87 = t11 - t27;
    t88 = t80 + t84;
    t89 = t81 + t85;
    t90 = t80 - t84;
    t91 = t81 - t85;
    t92 = t82 + t87;
    t93 = t83 - t86;
    t94 = t82 - t87;
    t95 = t83 + t86;
    t96 = t4 + t20;
    t97 = t5 + t21;
    t98 = t4 - t20;
    t99 = t5 - t21;
    t100 = t12 + t28;
    t101 = t13 + t29;
    t102 = t12 - t28;
    t103 = t13 - t29;
    t104 = t96 + t100;
    t105 = t97 + t101;
    t106 = t96 - t100;
    t107 = t97 - t101;
    t108 = t98 + t103;
    t109 = t99 - t102;
    t110 = t98 - t103;
    t111 = t99 + t102;
    t112 = t6 + t22;
    t113 = t7 + t23;
    t114 = t6 - t22;
    t115 = t7 - t23;
    t116 = t14 + t30;
    t117 = t15 + t31;
    t118 = t14 - t30;
    t119 = t15 - t31;
    t120 = t112 + t116;
    t121 = t113 + t117;
    t122 = t112 - t116;
    t123 = t113 - t117;
    t124 = t114 + t119;
    t125 = t115 - t118;
    t126 = t114 - t119;
    t127 = t115 + t118;
    t128 = t72 + t104;
    t129 = t73 + t105;
    t130 = t72 - t104;
    t131 = t73 - t105;
    t132 = t88 + t120;
    t133 = t89 + t121;
    t134 = t88 - t120;
    t135 = t89 - t121;
    t136 = t128 + t132;
    t137 = t129 + t133;
    t138 = t128 - t132;
    t139 = t129 - t133;
    t140 = t130 + t135;
    t141 = t131 - t134;
    t142 = t130 - t135;
    t143 = t131 + t134;
    t144 = 0.7071067811865475 * t108;
    t145 = 0.7071067811865475 * t109;
    t146 = t144 + t145;
    t147 = t76 + t146;
    t148 = t144 - t145;
    t149 = t77 - t148;
    t150 = t76 - t146;
    t151 = t77 + t148;
    t152 = 0.9238795325112867 * t92;
    t153 = 0.38268343236508984 * t93;
    t154 = t152 + t153;
    t155 = 0.38268343236508984 * t124;
    t156 = 0.9238795325112867 * t125;
    t157 = t155 + t156;
    t158 = t154 + t157;
    t159 = 0.38268343236508984 * t92;
    t160 = 0.9238795325112867 * t93;
    t161 = t159 - t160;
    t162 = 0.9238795325112867 * t124;
    t163 = 0.38268343236508984 * t125;
    t164 = t162 - t163;
    t165 = t161 + t164;
    t166 = t154 - t157;
    t167 = t161 - t164;
    t168 = t147 + t158;
    t169 = t149 - t165;
    t170 = t147 - t158;
    t171 = t149 + t165;
    t172 = t150 - t167;
    t173 = t151 - t166;
    t174 = t150 + t167;
    t175 = t151 + t166;
    t176 = t74 + t107;
    t177 = t75 - t106;
    t178 = t74 - t107;
    t179 = t75 + t106;
    t180 = 0.7071067811865475 * t90;
    t181 = 0.7071067811865475 * t91;
    t182 = t180 + t181;
    t183 = 0.7071067811865475 * t122;
    t184 = 0.7071067811865475 * t123;
    t185 = t183 - t184;
    t186 = t182 - t185;
    t187 = t180 - t181;
    t188 = t183 + t184;
    t189 = t187 + t188;
    t190 = t182 + t185;
    t191 = t187 - t188;
    t192 = t176 + t186;
    t193 = t177 - t189;
    t194 = t176 - t186;
    t195 = t177 + t189;
    t196 = t178 - t191;
    t197 = t179 - t190;
    t198 = t178 + t191;
    t199 = t179 + t190;
    t200 = 0.7071067811865475 * t110;
    t201 = 0.7071067811865475 * t111;
    t202 = t200 - t201;
    t203 = t78 - t202;
    t204 = t200 + t201;
    t205 = t79 - t204;
    t206 = t78 + t202;
    t207 = t79 + t204;
    t208 = 0.38268343236508984 * t94;
    t209 = 0.9238795325112867 * t95;
    t210 = t208 + t209;
    t211 = 0.9238795325112867 * t126;
    t212 = 0.38268343236508984 * t127;
    t213 = t211 + t212;
    t214 = t210 - t213;
    t215 = 0.9238795325112867 * t94;
    t216 = 0.38268343236508984 * t95;
    t217 = t215 - t216;
    t218 = 0.38268343236508984 * t126;
    t219 = 0.9238795325112867 * t127;
    t220 = t218 - t219;
    t221 = t217 - t220;
    t222 = t210 + t213;
    t223 = t217 + t220;
    t224 = t203 + t214;
    t225 = t205 - t221;
    t226 = t203 - t214;
    t227 = t205 + t221;
    t228 = t206 - t223;
    t229 = t207 - t222;
    t230 = t206 + t223;
    t231 = t207 + t222;
    t232 = t32 + t40;
    t233 = t33 + t41;
    t234 = t32 - t40;
    t235 = t33 - t41;
    t236 = t36 + t44;
    t237 = t37 + t45;
    t238 = t36 - t44;
    t239 = t37 - t45;
    t240 = t232 + t236;
    t241 = t233 + t237;
    t242 = t232 - t236;
    t243 = t233 - t237;
    t244 = t234 + t239;
    t245 = t235 - t238;
    t246 = t234 - t239;
    t247 = t235 + t238;
    t248 = t34 + t42;
    t249 = t35 + t43;
    t250 = t34 - t42;
    t251 = t35 - t43;
    t252 = t38 + t46;
    t253 = t39 + t47;
    t254 = t38 - t46;
    t255 = t39 - t47;
    t256 = t250 + t251;
    t257 = t250 - t251;
    t258 = t254 - t255;
    t259 = t254 + t255;
    t260 = t248 + t252;
    t261 = t249 + t253;
    t262 = t248 - t252;
    t263 = t249 - t253;
    t264 = t240 + t260;
    t265 = t241 + t261;
    t266 = t240 - t260;
    t267 = t241 - t261;
    t268 = t242 + t263;
    t269 = t243 - t262;
    t270 = t242 - t263;
    t271 = t243 + t262;
    t272 = t256 - t258;
    t273 = t257 + t259;
    t274 = t256 + t258;
    t275 = t257 - t259;
    t276 = t244 + t272;
    t277 = t245 - t273;
    t278 = t244 - t272;
    t279 = t245 + t273;
    t280 = t246 - t275;
    t281 = t247 - t274;
    t282 = t246 + t275;
    t283 = t247 + t274;
    t284 = t54 + t62;
    t285 = t55 + t63;
    t286 = t54 - t62;
    t287 = t55 - t63;
    t288 = t50 + t58;
    t289 = t51 + t59;
    t290 = t50 - t58;
    t291 = t51 - t59;
    t292 = t284 + t288;
    t293 = t285 + t289;
    t294 = t284 - t288;
    t295 = t285 - t289;
    t296 = t286 - t291;
    t297 = t287 + t290;
    t298 = t286 + t291;
    t299 = t287 - t290;
    t300 = t48 + t56;
    t301 = t49 + t57;
    t302 = t48 - t56;
    t303 = t49 - t57;
    t304 = t52 + t60;
    t305 = t53 + t61;
    t306 = t52 - t60;
    t307 = t53 - t61;
    t308 = t302 + t303;
    t309 = t302 - t303;
    t310 = t306 - t307;
    t311 = t306 + t307;
    t312 = t300 + t304;
    t313 = t301 + t305;
    t314 = t300 - t304;
    t315 = t301 - t305;
    t316 = t292 + t312;
    t317 = t293 + t313;
    t318 = t292 - t312;
    t319 = t293 - t313;
    t320 = t294 + t315;
    t321 = t295 - t314;
    t322 = t294 - t315;
    t323 = t295 + t314;
    t324 = t308 - t310;
    t325 = t309 + t311;
    t326 = t308 + t310;
    t327 = t309 - t311;
    t328 = t296 - t324;
    t329 = t297 + t325;
    t330 = t296 + t324;
    t331 = t297 - t325;
    t332 = t298 + t327;
    t333 = t299 + t326;
    t334 = t298 - t327;
    t335 = t299 - t326;
    t336 = 0.6935199226610737 * t276;
    t337 = 0.13794968964147147 * t277;
    t338 = t336 + t337;
    t339 = 0.13794968964147147 * t276;
    t340 = 0.6935199226610737 * t277;
    t341 = t339 - t340;
    t342 = 0.5879378012096793 * t280;
    t343 = 0.392847479193551 * t281;
    t344 = t342 + t343;
    t345 = 0.392847479193551 * t280;
    t346 = 0.5879378012096793 * t281;
    t347 = t345 - t346;
    t348 = 0.392847479193551 * t278;
    t349 = 0.5879378012096793 * t279;
    t350 = t348 + t349;
    t351 = 0.5879378012096793 * t278;
    t352 = 0.392847479193551 * t279;
    t353 = t351 - t352;
    t354 = 0.13794968964147147 * t282;
    t355 = 0.6935199226610737 * t283;
    t356 = t354 + t355;
    t357 = 0.6935199226610737 * t282;
    t358 = 0.13794968964147147 * t283;
    t359 = t357 - t358;
    t360 = 0.13794968964147147 * t329;
    t361 = 0.6935199226610737 * t328;
    t362 = t360 - t361;
    t363 = 0.13794968964147147 * t328;
    t364 = 0.6935199226610737 * t329;
    t365 = t363 + t364;
    t366 = 0.392847479193551 * t333;
    t367 = 0.5879378012096793 * t332;
    t368 = t366 - t367;
    t369 = 0.392847479193551 * t332;
    t370 = 0.5879378012096793 * t333;
    t371 = t369 + t370;
    t372 = 0.5879378012096793 * t331;
    t373 = 0.392847479193551 * t330;
    t374 = t372 - t373;
    t375 = 0.5879378012096793 * t330;
    t376 = 0.392847479193551 * t331;
    t377 = t375 + t376;
    t378 = 0.6935199226610737 * t335;
    t379 = 0.13794968964147147 * t334;
    t380 = t378 - t379;
    t381 = 0.6935199226610737 * t334;
    t382 = 0.13794968964147147 * t335;
    t383 = t381 + t382;
    t384 = t264 + t316;
    t385 = t265 + t317;
    t386 = t264 - t316;
    t387 = t265 - t317;
    t388 = t136 + t384;
    Y[0] = t388;
    t389 = t137 + t385;
    Y[1] = t389;
    t390 = t136 - t384;
    Y[32] = t390;
    t391 = t137 - t385;
    Y[33] = t391;
    t392 = t138 + t387;
    Y[16] = t392;
    t393 = t139 - t386;
    Y[17] = t393;
    t394 = t138 - t387;
    Y[48] = t394;
    t395 = t139 + t386;
    Y[49] = t395;
    t396 = t338 + t362;
    t397 = t341 + t365;
    t398 = t338 - t362;
    t399 = t341 - t365;
    t400 = t168 + t396;
    Y[2] = t400;
    t401 = t169 - t397;
    Y[3] = t401;
    t402 = t168 - t396;
    Y[34] = t402;
    t403 = t169 + t397;
    Y[35] = t403;
    t404 = t170 - t399;
    Y[18] = t404;
    t405 = t171 - t398;
    Y[19] = t405;
    t406 = t170 + t399;
    Y[50] = t406;
    t407 = t171 + t398;
    Y[51] = t407;
    t408 = 0.9238795325112867 * t268;
    t409 = 0.38268343236508984 * t269;
    t410 = t408 + t409;
    t411 = 0.9238795325112867 * t320;
    t412 = 0.38268343236508984 * t321;
    t413 = t411 - t412;
    t414 = t410 + t413;
    t415 = 0.38268343236508984 * t268;
    t416 = 0.9238795325112867 * t269;
    t417 = t415 - t416;
    t418 = 0.38268343236508984 * t320;
    t419 = 0.9238795325112867 * t321;
    t420 = t418 + t419;
    t421 = t417 - t420;
    t422 = t410 - t413;
    t423 = t417 + t420;
    t424 = t192 + t414;
    Y[4] = t424;
    t425 = t193 - t421;
    Y[5] = t425;
    t426 = t192 - t414;
    Y[36] = t426;
    t427 = t193 + t421;
    Y[37] = t427;
    t428 = t194 - t423;
    Y[20] = t428;
    t429 = t195 - t422;
    Y[21] = t429;
    t430 = t194 + t423;
    Y[52] = t430;
    t431 = t195 + t422;
    Y[53] = t431;
    t432 = t344 + t368;
    t433 = t347 + t371;
    t434 = t344 - t368;
    t435 = t347 - t371;
    t436 = t224 + t432;
    Y[6] = t436;
    t437 = t225 - t433;
    Y[7] = t437;
    t438 = t224 - t432;
    Y[38] = t438;
    t439 = t225 + t433;
    Y[39] = t439;
    t440 = t226 - t435;
    Y[22] = t440;
    t441 = t227 - t434;
    Y[23] = t441;
    t442 = t226 + t435;
    Y[54] = t442;
    t443 = t227 + t434;
    Y[55] = t443;
    t444 = 0.7071067811865475 * t266;
    t445 = 0.7071067811865475 * t267;
    t446 = t444 + t445;
    t447 = 0.7071067811865475 * t318;
    t448 = 0.7071067811865475 * t319;
    t449 = t447 - t448;
    t450 = t446 + t449;
    t451 = t444 - t445;
    t452 = t447 + t448;
    t453 = t451 - t452;
    t454 = t446 - t449;
    t455 = t451 + t452;
    t456 = t140 + t450;
    Y[8] = t456;
    t457 = t141 - t453;
    Y[9] = t457;
    t458 = t140 - t450;
    Y[40] = t458;
    t459 = t141 + t453;
    Y[41] = t459;
    t460 = t142 - t455;
    Y[24] = t460;
    t461 = t143 - t454;
    Y[25] = t461;
    t462 = t142 + t455;
    Y[56] = t462;
    t463 = t143 + t454;
    Y[57] = t463;
    t464 = t350 + t374;
    t465 = t353 + t377;
    t466 = t350 - t374;
    t467 = t353 - t377;
    t468 = t172 + t464;
    Y[10] = t468;
    t469 = t173 - t465;
    Y[11] = t469;
    t470 = t172 - t464;
    Y[42] = t470;
    t471 = t173 + t465;
    Y[43] = t471;
    t472 = t174 - t467;
    Y[26] = t472;
    t473 = t175 - t466;
    Y[27] = t473;
    t474 = t174 + t467;
    Y[58] = t474;
    t475 = t175 + t466;
    Y[59] = t475;
    t476 = 0.38268343236508984 * t270;
    t477 = 0.9238795325112867 * t271;
    t478 = t476 + t477;
    t479 = 0.38268343236508984 * t322;
    t480 = 0.9238795325112867 * t323;
    t481 = t479 - t480;
    t482 = t478 + t481;
    t483 = 0.9238795325112867 * t270;
    t484 = 0.38268343236508984 * t271;
    t485 = t483 - t484;
    t486 = 0.9238795325112867 * t322;
    t487 = 0.38268343236508984 * t323;
    t488 = t486 + t487;
    t489 = t485 - t488;
    t490 = t478 - t481;
    t491 = t485 + t488;
    t492 = t196 + t482;
    Y[12] = t492;
    t493 = t197 - t489;
    Y[13] = t493;
    t494 = t196 - t482;
    Y[44] = t494;
    t495 = t197 + t489;
    Y[45] = t495;
    t496 = t198 - t491;
    Y[28] = t496;
    t497 = t199 - t490;
    Y[29] = t497;
    t498 = t198 + t491;
    Y[60] = t498;
    t499 = t199 + t490;
    Y[61] = t499;
    t500 = t356 + t380;
    t501 = t359 + t383;
    t502 = t356 - t380;
    t503 = t359 - t383;
    t504 = t228 + t500;
    Y[14] = t504;
    t505 = t229 - t501;
    Y[15] = t505;
    t506 = t228 - t500;
    Y[46] = t506;
    t507 = t229 + t501;
    Y[47] = t507;
    t508 = t230 - t503;
    Y[30] = t508;
    t509 = t231 - t502;
    Y[31] = t509;
    t510 = t230 + t503;
    Y[62] = t510;
    t511 = t231 + t502;
    Y[63] = t511;
}