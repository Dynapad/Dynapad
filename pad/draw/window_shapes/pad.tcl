
#
# JM - window Shape coordinates (these are the coordinates of two
# pad shapes, one for the inner area of the window, one for the border). The
# .pad shape command takes care of mapping these to window coordinates.
# 

set inner_coords {
-94.364433 -69.021446 -94.288536 -73.825150 -94.206230 -74.541847 -94.747231 -79.953651 -92.118233 -80.334351 -92.118233 -80.334351 -91.728630 -79.247452 -90.375832 -78.776253 -89.499229 -78.894653 -89.499229 -78.894653 -88.622932 -79.013153 -87.132835 -80.078247 -86.983330 -81.378853 -86.983330 -81.378853 -86.833931 -82.679153 -87.490532 -84.170547 -88.586136 -84.022453 -88.586136 -84.022453 -89.681831 -83.874550 -90.914131 -83.475151 -91.063530 -82.174751 -94.212929 -81.989349 -95.178032 -87.079247 -91.617332 -88.617249 -91.617332 -88.617249 -91.657234 -87.331749 -90.382530 -86.223747 -89.506035 -86.342148 -89.506035 -86.342148 -88.629433 -86.460648 -87.139732 -87.525848 -86.990334 -88.826347 -86.990334 -88.826347 -86.840836 -90.126648 -87.497429 -91.618050 -88.593132 -91.469948 -88.593132 -91.469948 -89.688530 -91.321953 -90.921036 -90.922546 -91.070335 -89.622253 -94.768532 -89.311249 -94.308830 -96.167648 -91.624229 -96.064751 -91.624229 -96.064751 -91.664131 -94.779152 -90.389229 -93.671249 -89.512733 -93.789650 -89.512733 -93.789650 -88.636131 -93.908150 -87.146431 -94.973351 -86.996834 -96.273849 -86.996834 -96.273849 -86.847435 -97.574150 -87.504234 -99.065552 -88.599632 -98.917450 -88.599632 -98.917450 -89.695335 -98.769547 -90.927834 -98.370049 -91.088936 -97.439751 -93.778435 -97.389053 -94.315636 -103.615150 -91.630936 -103.512451 -91.630936 -103.512451 -91.670830 -102.226646 -90.396233 -101.118752 -89.519630 -101.237152 -89.519630 -101.237152 -88.643135 -101.355652 -87.153336 -102.420753 -87.003731 -103.721352 -87.003731 -103.721352 -86.854332 -105.021652 -87.510933 -106.513046 -88.606529 -106.364952 -88.606529 -106.364952 -89.702034 -106.216949 -90.934532 -105.817749 -91.083931 -104.517250 -94.967636 -104.297653 -94.322334 -111.062752 -91.637733 -110.959747 -91.637733 -110.959747 -91.677536 -109.674149 -90.402733 -108.566254 -89.526329 -108.684647 -89.526329 -108.684647 -88.649834 -108.803146 -87.159935 -109.868446 -87.010429 -111.168854 -87.010429 -111.168854 -86.861031 -112.469147 -87.517632 -113.960548 -88.613335 -113.812447 -88.613335 -113.812447 -89.708931 -113.664452 -90.941231 -113.265251 -91.090836 -111.964851 -93.680931 -112.508347 -94.329231 -118.510048 -91.644432 -118.407448 -91.644432 -118.407448 -91.684334 -117.121651 -90.409431 -116.013947 -89.533134 -116.132149 -89.533134 -116.132149 -88.656532 -116.250549 -87.166832 -117.315948 -87.017136 -118.616348 -87.017136 -118.616348 -86.867729 -119.916649 -87.524536 -121.408051 -88.620033 -121.260147 -88.620033 -121.260147 -89.715630 -121.111954 -90.948036 -120.712753 -91.143333 -119.751350 -93.949532 -120.340652 -94.335732 -125.957748 -91.651131 -125.854950 -91.651131 -125.854950 -91.691231 -124.569153 -90.416336 -123.461349 -89.539833 -123.579651 -89.539833 -123.579651 -88.663231 -123.698051 -87.173531 -124.763451 -87.023933 -126.063850 -87.023933 -126.063850 -86.874535 -127.364349 -87.531136 -128.855545 -88.626930 -128.707657 -88.626930 -128.707657 -89.722435 -128.559448 -90.954735 -128.160248 -91.111229 -127.382652 -93.185631 -127.127151 -94.342636 -133.405243 -91.657936 -133.302444 -91.657936 -133.302444 -91.697929 -132.016846 -90.423134 -130.908844 -89.546730 -131.027344 -89.546730 -131.027344 -88.670029 -131.145554 -87.180229 -132.210953 -87.030830 -133.511246 -87.030830 -133.511246 -86.881432 -134.811844 -87.538033 -136.303055 -88.633430 -136.155045 -88.633430 -136.155045 -89.729134 -136.006943 -90.961632 -135.607758 -91.111031 -134.307343 -94.260231 -134.121948 -94.371834 -139.732254 -91.742729 -140.113144 -91.742729 -140.113144 -91.782631 -138.827545 -90.429832 -138.356354 -89.553429 -138.474854 -89.553429 -138.474854 -88.676735 -138.593048 -87.186829 -139.658447 -87.037529 -140.958954 -87.037529 -140.958954 -86.887932 -142.259354 -87.544533 -143.750549 -88.640236 -143.602554 -88.640236 -143.602554 -89.736031 -143.454453 -90.968132 -143.055252 -91.117729 -141.754852 -94.266930 -141.569443 -95.232231 -146.659348 -91.671532 -148.197449 -91.671532 -148.197449 -91.711433 -146.911850 -90.436531 -145.803848 -89.559929 -145.922348 -89.559929 -145.922348 -88.683632 -146.040756 -87.193932 -147.105957 -87.044334 -148.406448 -87.044334 -148.406448 -86.894829 -149.706757 -87.551430 -151.198044 -88.647133 -151.050049 -88.647133 -151.050049 -89.742729 -150.901947 -90.975029 -150.502747 -91.124535 -149.202347 -94.822731 -148.891357 -94.363029 -155.747757 -91.678230 -155.644943 -91.678230 -155.644943 -91.718132 -154.359344 -90.443436 -153.251343 -89.566734 -153.369843 -89.566734 -153.369843 -88.690331 -153.488251 -87.200432 -154.553452 -87.051033 -155.853943 -87.051033 -155.853943 -86.901436 -157.154343 -87.558136 -158.645645 -88.653831 -158.497543 -88.653831 -158.497543 -89.749229 -158.349457 -90.981636 -157.950256 -91.143036 -157.019745 -93.832436 -156.969147 -94.369736 -163.195251 -91.685036 -163.092346 -91.685036 -163.092346 -91.725029 -161.806854 -90.450035 -160.698853 -89.573631 -160.817352 -89.573631 -160.817352 -88.697136 -160.935745 -87.207130 -162.000946 -87.057732 -163.301453 -87.057732 -163.301453 -86.908333 -164.601746 -87.565132 -166.093155 -88.660530 -165.945053 -88.660530 -165.945053 -89.756233 -165.796951 -90.988731 -165.397751 -91.138130 -164.097351 -95.021935 -163.877853 -94.376335 -170.642746 -91.691734 -170.539948 -91.691734 -170.539948 -91.731529 -169.254349 -90.456932 -168.146347 -89.580330 -168.264755 -89.580330 -168.264755 -88.703835 -168.383255 -87.214134 -169.448547 -87.064629 -170.748947 -87.064629 -170.748947 -86.915031 -172.049255 -87.571831 -173.540649 -88.667336 -173.392548 -88.667336 -173.392548 -89.763130 -173.244644 -90.995430 -172.845245 -91.144829 -171.544846 -93.496529 -171.753250 -93.585732 -178.484055 -91.698631 -177.987457 -91.698631 -177.987457 -91.738533 -176.701843 -90.463631 -175.594055 -89.587135 -175.712250 -89.587135 -175.712250 -88.710732 -175.830750 -87.220833 -176.896057 -87.071129 -178.196457 -87.071129 -178.196457 -86.921730 -179.496750 -87.578331 -180.988144 -88.674034 -180.840057 -88.674034 -180.840057 -89.769630 -180.692154 -91.002129 -180.292755 -91.197533 -179.331345 -93.260735 -180.020844 -94.389931 -185.537750 -91.705132 -185.434845 -91.705132 -185.434845 -91.745033 -184.149353 -90.470436 -183.041550 -89.593834 -183.159744 -89.593834 -183.159744 -88.717430 -183.278244 -87.227531 -184.343552 -87.078133 -185.643951 -87.078133 -185.643951 -86.928535 -186.944244 -87.585335 -188.435654 -88.680931 -188.287552 -88.680931 -188.287552 -89.776634 -188.139648 -91.008736 -187.740250 -91.158333 -186.439850 -93.232735 -186.184357 -94.269234 -191.668243 -94.269234 -191.668243 -93.591583 -191.050446 -92.852333 -190.556244 -91.604843 -190.216507 -91.604843 -190.216507 -90.357361 -189.876755 -88.601631 -189.691452 -85.891029 -189.691452 -85.891029 -189.691452 -80.469933 -189.691452 -58.292736 -191.668243 -51.885937 -190.679947 -51.885937 -190.679947 -51.488392 -190.618607 -50.699371 -190.551575 -49.565395 -190.480865 -49.565395 -190.480865 -48.431423 -190.410141 -46.952492 -190.335724 -45.175133 -190.259628 -45.175133 -190.259628 -41.620411 -190.107452 -36.871964 -189.948532 -31.301971 -189.798920 -31.301971 -189.798920 -25.731977 -189.649307 -19.340441 -189.509018 -12.499542 -189.394073 -12.499542 -189.394073 -5.658642 -189.279129 1.631620 -189.189545 8.999063 -189.141357 8.999063 -189.141357 23.733952 -189.044968 38.777565 -189.114197 51.152447 -189.477386 51.152447 -189.477386 57.339890 -189.658966 62.860149 -189.914047 67.341049 -190.258667 67.341049 -190.258667 71.821945 -190.603271 75.263481 -191.037430 77.293465 -191.577148 77.293465 -191.577148 77.276665 -184.305252 77.137169 -177.233856 77.009270 -169.760651 77.009270 -169.760651 76.634666 -158.645645 75.950966 -148.878952 75.292564 -138.423248 75.292564 -138.423248 74.742165 -132.131546 73.415764 -125.749748 73.666664 -118.859451 73.666664 -118.859451 73.734169 -109.871849 73.542671 -100.854446 74.645271 -91.987152 74.645271 -91.987152 75.735268 -84.096748 77.408165 -76.684250 77.460869 -68.861053 -94.364433 -69.021446
}

set outer_coords {
-93.733101 -69.021446 -93.715103 -70.165550 -93.715103 -70.165550 -94.085800 -70.165352 -94.429802 -70.167549 -94.718697 -70.174454 -94.651299 -74.435051 -94.560600 -75.695053 -95.101601 -81.106651 -92.472298 -81.487648 -92.472298 -81.487648 -92.082703 -80.400650 -90.730103 -79.929550 -89.853699 -80.047951 -89.853699 -80.047951 -88.976997 -80.166351 -87.487099 -81.231552 -87.337700 -82.531952 -87.337700 -82.531952 -87.282097 -83.016953 -87.338997 -83.528053 -87.488403 -83.971252 -87.488403 -83.971252 -87.634102 -84.018753 -87.787300 -84.045052 -87.954697 -84.022453 -87.954697 -84.022453 -89.050400 -83.874550 -90.282898 -83.475151 -90.432198 -82.174751 -93.581596 -81.989349 -93.808899 -83.187149 -94.567299 -83.142548 -95.448799 -87.791451 -90.182198 -88.575851 -90.182198 -88.575851 -89.796097 -88.050247 -89.178497 -87.705048 -88.476303 -87.705048 -88.476303 -87.705048 -87.304802 -87.705048 -86.355003 -88.654846 -86.355003 -89.826347 -86.355003 -89.826347 -86.355003 -90.997849 -87.304802 -91.947647 -88.476303 -91.947647 -88.476303 -91.947647 -89.647903 -91.947647 -90.597702 -90.997849 -90.597702 -89.826347 -90.597702 -89.826347 -90.597702 -89.663353 -90.575500 -89.506248 -90.540604 -89.353752 -93.198402 -89.240646 -92.768303 -89.426453 -94.137199 -89.311249 -94.054001 -90.554451 -95.122902 -90.464447 -94.663200 -97.320847 -91.978500 -97.218048 -91.978500 -97.218048 -91.991096 -96.811546 -91.863098 -96.428551 -91.667000 -96.090752 -90.992798 -96.064949 -90.992798 -96.064949 -91.002197 -95.765350 -90.934502 -95.477249 -90.822701 -95.210152 -90.822701 -95.210152 -90.502197 -95.011353 -90.162697 -94.903053 -89.866997 -94.942947 -89.866997 -94.942947 -88.990601 -95.061348 -87.500801 -96.126549 -87.351196 -97.427048 -87.351196 -97.427048 -87.295403 -97.911949 -87.352402 -98.423050 -87.501900 -98.866249 -87.501900 -98.866249 -87.647697 -98.913750 -87.800903 -98.940048 -87.968300 -98.917450 -87.968300 -98.917450 -89.064003 -98.769547 -90.296501 -98.370049 -90.457703 -97.439751 -93.147102 -97.389053 -93.248100 -98.558952 -94.132797 -98.542351 -94.596603 -103.917648 -90.411301 -103.680054 -90.411301 -103.680054 -90.140099 -103.086952 -89.605499 -102.623451 -88.917900 -102.480850 -88.917900 -102.480850 -87.770798 -102.242851 -86.647903 -102.979851 -86.409897 -104.126953 -86.409897 -104.126953 -86.171898 -105.274048 -86.908997 -106.396950 -88.056099 -106.634949 -88.056099 -106.634949 -89.203300 -106.872948 -90.326103 -106.135948 -90.564201 -104.988853 -90.564201 -104.988853 -90.597298 -104.829247 -90.607399 -104.670853 -90.604301 -104.514450 -94.258797 -105.111954 -94.220398 -105.513046 -95.322197 -105.450752 -94.676697 -112.215851 -91.991997 -112.113052 -91.991997 -112.113052 -92.004501 -111.706451 -91.876701 -111.323547 -91.680397 -110.985748 -91.006401 -110.959953 -91.006401 -110.959953 -91.015602 -110.660347 -90.948303 -110.372353 -90.836304 -110.105347 -90.836304 -110.105347 -90.515800 -109.906448 -90.176201 -109.798050 -89.880798 -109.837952 -89.880798 -109.837952 -89.004097 -109.956352 -87.514198 -111.021553 -87.364799 -112.322052 -87.364799 -112.322052 -87.309196 -112.806953 -87.366096 -113.318047 -87.515297 -113.761253 -87.515297 -113.761253 -87.661003 -113.808647 -87.814400 -113.835152 -87.981796 -113.812653 -87.981796 -113.812653 -89.077499 -113.664452 -90.309998 -113.265251 -90.459297 -111.964752 -93.049500 -112.508347 -93.153999 -113.476646 -94.035202 -113.661453 -94.683403 -119.663353 -91.998901 -119.560654 -91.998901 -119.560654 -92.011299 -119.153954 -91.883400 -118.771049 -91.687302 -118.433250 -91.013000 -118.407448 -91.013000 -118.407448 -91.022400 -118.107750 -90.954903 -117.819847 -90.843002 -117.552849 -90.843002 -117.552849 -90.522400 -117.353752 -90.183098 -117.245552 -89.887398 -117.285454 -89.887398 -117.285454 -89.010803 -117.403847 -87.521004 -118.469048 -87.371498 -119.769547 -87.371498 -119.769547 -87.315804 -120.254448 -87.372597 -120.765549 -87.521896 -121.208649 -87.521896 -121.208649 -87.667900 -121.256248 -87.821098 -121.282654 -87.988701 -121.260147 -87.988701 -121.260147 -89.084297 -121.111954 -90.316704 -120.712753 -90.511902 -119.751350 -93.318100 -120.340652 -93.384399 -121.300652 -94.303902 -121.493950 -94.690102 -127.111053 -92.005600 -127.008148 -92.005600 -127.008148 -92.018204 -126.601646 -91.890198 -126.218750 -91.694099 -125.880753 -91.019897 -125.854950 -91.019897 -125.854950 -91.029099 -125.555252 -90.961601 -125.267349 -90.850098 -125.000351 -90.850098 -125.000351 -90.529404 -124.801453 -90.189796 -124.693047 -89.894096 -124.732948 -89.894096 -124.732948 -89.017700 -124.851349 -87.527702 -125.916748 -87.378304 -127.217049 -87.378304 -127.217049 -87.322502 -127.701950 -87.379402 -128.213043 -87.528999 -128.656143 -87.528999 -128.656143 -87.674599 -128.703644 -87.828003 -128.730148 -87.995598 -128.707657 -87.995598 -128.707657 -89.091103 -128.559448 -90.323402 -128.160248 -90.479797 -127.382652 -92.554199 -127.127151 -92.783997 -128.373352 -93.539902 -128.280243 -94.607903 -134.075546 -90.578003 -133.846756 -90.578003 -133.846756 -90.306702 -133.253555 -89.772102 -132.790146 -89.084602 -132.647446 -89.084602 -132.647446 -87.937500 -132.409546 -86.814499 -133.146545 -86.576599 -134.293655 -86.576599 -134.293655 -86.338600 -135.440750 -87.075600 -136.563644 -88.222702 -136.801651 -88.222702 -136.801651 -89.369904 -137.039658 -90.492798 -136.302643 -90.730797 -135.155548 -90.730797 -135.155548 -90.763901 -134.995956 -90.774101 -134.837555 -90.770897 -134.681152 -93.649399 -135.151749 -93.653000 -135.331650 -94.459602 -135.284256 -94.615196 -135.309555 -94.726196 -140.885452 -92.096901 -141.266357 -92.096901 -141.266357 -92.113998 -140.715942 -91.869499 -140.323257 -91.522499 -140.053543 -91.111397 -140.113144 -91.111397 -140.113144 -91.114998 -139.997757 -91.095398 -139.896454 -91.078003 -139.793945 -91.078003 -139.793945 -90.691399 -139.634354 -90.261902 -139.580048 -89.907600 -139.627945 -89.907600 -139.627945 -89.031197 -139.746353 -87.541199 -140.811646 -87.391701 -142.112045 -87.391701 -142.112045 -87.335999 -142.597153 -87.392998 -143.108047 -87.542397 -143.551147 -87.542397 -143.551147 -87.688103 -143.598648 -87.841499 -143.625153 -88.008904 -143.602554 -88.008904 -143.602554 -89.104599 -143.454453 -90.336800 -143.055252 -90.486397 -141.754852 -93.635803 -141.569443 -93.862801 -142.767242 -94.621399 -142.722549 -95.586304 -147.812653 -93.911697 -148.535950 -90.578003 -148.346756 -90.578003 -148.346756 -90.306702 -147.753555 -89.772102 -147.290146 -89.084602 -147.147446 -89.084602 -147.147446 -87.937500 -146.909546 -86.814499 -147.646545 -86.576599 -148.793655 -86.576599 -148.793655 -86.338600 -149.940750 -87.075600 -151.063644 -88.222702 -151.301651 -88.222702 -151.301651 -89.369904 -151.539658 -90.492798 -150.802643 -90.730797 -149.655548 -90.730797 -149.655548 -90.763901 -149.495956 -90.774101 -149.337555 -90.770897 -149.181152 -94.135201 -149.731155 -94.108101 -150.134552 -95.177002 -150.044647 -94.717201 -156.900955 -92.032402 -156.798157 -92.032402 -156.798157 -92.045097 -156.391647 -91.917297 -156.008652 -91.721001 -155.670654 -91.046997 -155.644943 -91.046997 -155.644943 -91.056198 -155.345444 -90.988701 -155.057343 -90.876900 -154.790253 -90.876900 -154.790253 -90.556198 -154.591446 -90.216797 -154.483047 -89.921204 -154.522842 -89.921204 -154.522842 -89.044800 -154.641449 -87.554802 -155.706650 -87.405403 -157.007248 -87.405403 -157.007248 -87.349602 -157.492157 -87.406502 -158.003143 -87.555901 -158.446350 -87.555901 -158.446350 -87.701599 -158.493851 -87.854797 -158.520157 -88.022400 -158.497543 -88.022400 -158.497543 -89.117897 -158.349457 -90.350403 -157.950256 -90.511597 -157.019745 -93.201103 -156.969147 -93.302101 -158.139053 -94.186897 -158.122452 -94.621101 -163.156647 -90.755699 -163.566544 -90.755699 -163.566544 -90.392097 -163.025055 -89.789597 -162.654144 -89.088097 -162.624451 -89.088097 -162.624451 -87.917603 -162.575043 -86.928596 -163.483856 -86.879097 -164.654343 -86.879097 -164.654343 -86.829697 -165.824844 -87.738503 -166.813843 -88.908997 -166.863358 -88.908997 -166.863358 -90.079597 -166.912750 -91.068604 -166.003952 -91.117996 -164.833450 -91.117996 -164.833450 -91.124901 -164.670547 -91.109299 -164.512650 -91.081001 -164.358856 -94.344803 -164.357849 -94.274399 -165.093155 -95.376198 -165.030853 -94.730698 -171.795944 -92.295097 -171.702850 -92.865303 -171.753250 -92.865303 -171.754944 -90.238098 -171.691147 -90.238098 -171.691147 -89.947701 -171.107056 -89.398300 -170.661255 -88.706497 -170.540955 -88.706497 -170.540955 -87.552299 -170.340256 -86.453796 -171.113358 -86.253197 -172.267654 -86.253197 -172.267654 -86.052498 -173.421844 -86.825600 -174.520248 -87.979797 -174.720947 -87.979797 -174.720947 -89.134102 -174.921555 -90.232498 -174.148544 -90.433197 -172.994247 -90.433197 -172.994247 -90.461098 -172.833649 -90.466103 -172.675049 -90.457901 -172.518845 -93.851601 -172.961243 -93.940102 -179.637253 -92.052803 -179.140656 -92.052803 -179.140656 -92.063301 -178.805557 -91.981499 -178.484146 -91.843300 -178.191650 -91.067200 -177.987457 -91.067200 -177.987457 -91.076302 -177.687943 -91.008797 -177.400055 -90.897202 -177.132751 -90.897202 -177.132751 -90.576599 -176.933945 -90.237297 -176.825653 -89.941399 -176.865555 -89.941399 -176.865555 -89.065002 -176.983948 -87.575203 -178.049149 -87.425598 -179.349655 -87.425598 -179.349655 -87.370003 -179.834656 -87.426697 -180.345657 -87.576103 -180.788849 -87.576103 -180.788849 -87.721802 -180.836349 -87.875099 -180.862747 -88.042702 -180.840057 -88.042702 -180.840057 -89.138298 -180.692154 -90.370796 -180.292847 -90.565903 -179.331543 -92.629402 -180.020844 -92.810303 -180.905243 -93.614998 -181.174149 -94.731697 -186.629456 -90.404701 -186.524445 -90.404701 -186.524445 -90.114304 -185.940353 -89.565002 -185.494553 -88.873199 -185.374252 -88.873199 -185.374252 -87.718903 -185.173645 -86.620499 -185.946655 -86.419800 -187.100952 -86.419800 -187.100952 -86.219200 -188.255157 -86.992302 -189.353546 -88.146500 -189.554245 -88.146500 -189.554245 -89.300797 -189.754852 -90.399200 -188.981857 -90.599800 -187.827652 -90.599800 -187.827652 -90.627800 -187.667053 -90.632797 -187.508347 -90.624496 -187.352158 -93.664902 -187.748444 -94.623398 -192.821457 -94.623398 -192.821457 -93.945801 -192.203705 -93.206573 -191.709503 -91.959114 -191.369736 -91.959114 -191.369736 -90.711647 -191.029968 -88.955948 -190.844650 -86.245399 -190.844650 -86.245399 -190.844650 -80.824402 -190.844650 -58.647099 -192.821457 -52.240299 -191.832947 -52.240299 -191.832947 -51.839874 -191.771179 -51.042107 -191.703613 -49.894562 -191.632324 -49.894562 -191.632324 -48.747017 -191.561020 -47.249691 -191.485977 -45.450146 -191.409241 -45.450146 -191.409241 -41.851059 -191.255783 -37.043102 -191.095551 -31.406773 -190.944977 -31.406773 -190.944977 -25.770443 -190.794403 -19.305740 -190.653488 -12.393162 -190.538620 -12.393162 -190.538620 -5.480584 -190.423752 1.879869 -190.334961 9.307700 -190.288620 9.307700 -190.288620 24.163363 -190.195969 39.288536 -190.273178 51.639248 -190.651566 51.639248 -190.651566 57.814602 -190.840744 63.296341 -191.105225 67.703964 -191.461411 67.703964 -191.461411 72.111588 -191.817581 75.445099 -192.265457 77.323997 -192.821457 77.323997 -191.397751 77.323997 -191.397751 77.526199 -191.451752 77.724602 -191.507751 77.924797 -191.562653 77.924797 -191.562653 77.907898 -184.296051 77.768501 -177.229156 77.640701 -169.760651 77.640701 -169.760651 77.265999 -158.645645 76.582397 -148.878952 75.923897 -138.423248 75.923897 -138.423248 75.373497 -132.131546 74.046997 -125.749748 74.297997 -118.859451 74.297997 -118.859451 74.365501 -109.871849 74.174004 -100.854446 75.276604 -91.987152 75.276604 -91.987152 76.366699 -84.096947 78.039497 -76.684250 78.092300 -68.861252 -93.733101 -69.021446
}










