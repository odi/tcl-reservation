var reservationApp = angular.module('reservationApp', ['bootstrap3-typeahead','angularUtils.directives.dirPagination']);

var curDate = "";
var saveError = null;
var saveSuccess = null;
var saveStatus = null;

// local development
// var url = "http://127.0.0.1:8000"
// var rootDir = "/";

// remote dunkl.ddns.net -> Test instance
var url = "http://dunkl.ddns.net/tclaa-test"
var rootDir = "/tclaa-test/"

// remote dunkl.ddns.net -> Production instance
// var url = "http://dunkl.ddns.net/tclaa
// var rootDir = "/tclaa

// misc
//var url = "http://192.168.1.100:8000"
//var url = "http://192.168.1.10:8000"
//var url = "http://10.233.1.2:8000"

moment.locale("de");

function getParameterByName(name, url) {
    if (!url) url = window.location.href;
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}

reservationApp.filter('unique', function () {
    return function (collection, keyname) {
	var output = [],
	    keys   = [];
	angular.forEach(collection, function (item) {
	    var key = item[keyname];
	    if (keys.indexOf(key) == -1) {
		keys.push(key);
		output.push(item);
	    }
	});
	return output;
    };
});

/*
reservationApp.config(function($httpProvider) {
    $httpProvider.defaults.useXDomain = true;
    delete $httpProvider.defaults.headers.common['X-Requested-With'];
});
*/

reservationApp.controller('DateCtrl', ['$scope', function($scope) {
    var param = window.location.search.split('date=')[1];
    curDate = param;
    $scope.selectedDate = curDate;
}]);

reservationApp.controller('ReservationListCtrl', ['$scope', '$http', function($scope, $http) {

    $scope.status = getParameterByName('status');
    $scope.msg = getParameterByName('msg');
    $scope.rootDir = rootDir;

    function str_pad(n) {
	return String("00" + n).slice(-2);
    }

    $scope.getList = function (date) {
	var request = url + "/get/resbydate"
	var curDate = null;
	if (date == null) {
	    curDate = document.getElementById('datetimepicker1Value').value;
	} else {
	    curDate = date;
	}
	$scope.titleDate = moment(curDate).format('dddd DD.MM.YYYY');
	if (curDate != undefined) {
	    request = request + "?date=" + curDate
	}
	$http.get(request).success(function(data) {
	    $scope.reservations = data;
	    for (i=0; i<$scope.reservations.length; i++) {
		var time = $scope.reservations[i].startTime;
		var hour = time.split(':')[0];
		var minute = time.split(':')[1];
		$scope.reservations[i].startTime = str_pad(hour) + ":" + minute;
	    };
	});
    };

    $scope.addDate = function () {
	var curDate = document.getElementById('datetimepicker1Value').value;
	var newDate = moment(curDate).add(1,'days').format("YYYY-MM-DD");
	$scope.getList(newDate);
	document.getElementById('datetimepicker1Value').value = newDate;
    }

    $scope.removeDate = function () {
	var curDate = document.getElementById('datetimepicker1Value').value;
	var newDate = moment(curDate).add(-1,'days').format("YYYY-MM-DD");
	$scope.getList(newDate);
	document.getElementById('datetimepicker1Value').value = newDate;
    }
    
    $scope.currentLoginName = null;
    var currentLogin = function () {
	$http.get(url + "/auth/current").success(function (data) {
	    if (data != "error") {
		$scope.currentLoginName = data;
	    }
	});
    };

    var getUserRes = function () {
	$http.get(url + "/get/resbyuser").success(function (data) {
	    $scope.userReservations = [];
	    var ret = data;
	    for (i=0; i<ret.length; i++) {
		var date = moment(ret[i].date + " " + ret[i].startTime);
		var now = moment(new Date());
		if (date > now) {
		    $scope.userReservations.push(ret[i]);
		}
	    }
	});
    };

    $scope.deleteRes = function(resId) {
	$http.get(url + "/del/reservation?resId=" + resId).then(function successCallback(res) {
	    window.location = url;
	}, function errorCallback(res) {
	    window.location = url;
	});
    };

    var init = function () {
	$http.get(url + "/auth/current").success(function (data) {
	    if (data != "error") {
		$scope.currentLoginName = data;
		$http.get(url + "/get/resbyuser").success(function (data) {
		    $scope.userReservations = [];
		    var ret = data;
		    for (i=0; i<ret.length; i++) {
			var date = moment(ret[i].date + " " + ret[i].startTime);
			var now = moment(new Date());
			if (date > now) {
			    $scope.userReservations.push(ret[i]);
			}
		    }
		    $scope.getList(moment(new Date()).format("YYYY-MM-DD"));
		});
	    }
	});
    };

    init();
//    currentLogin();
//    getUserRes();
//    $scope.getList(moment(new Date()).format("YYYY-MM-DD"));
    
}]);

reservationApp.controller('ReservationCtrl', ['$scope', '$http', function($scope, $http) {
    $scope.rootDir = rootDir;
    $scope.loadPage = 0;
    $scope.nrPlayers = [];
    $scope.reservationTyp = 1;
    $scope.reservationTypSelect = 'Einzel';
    $scope.nrPlayers = [ {nr: 1, info: 'Spieler 1'},
			 {nr: 2, info: 'Spieler 2'} ];

    $scope.update = function () {
	switch($scope.reservationTypSelect) {
	case 'Einzel':
	    $scope.nrPlayers = [ {nr: 1, info: 'Spieler 1'},
				 {nr: 2, info: 'Spieler 2'} ]
	    $scope.reservationTyp = 1;
	    break;
	case 'Doppel':
	    $scope.nrPlayers = [ {nr: 1, info: ''},
				 {nr: 2, info: ''},
				 {nr: 3, info: ''},
				 {nr: 4, info:''} ]
	    $scope.reservationTyp = 2;
	    break;
	case 'Training':
	    $scope.nrPlayers = [ {nr: 1, info: 'Ansprechperson für das Training' } ]
	    $scope.reservationTyp = 3;
	    break;
	case 'Meisterschaft':
	    $scope.nrPlayers = [ {nr: 1, info: 'Ansprechperson für die Meisterschaft' } ]
	    $scope.reservationTyp = 4;
	    break
	case 'Ranglistenmatch':
	    $scope.nrPlayers = [ {nr: 1, info: 'Spieler 1'},
				 {nr: 2, info: 'Spieler 2'} ]
	    $scope.reservationTyp = 5;
	    break;
	default:
	    $scope.nrPlayers = [ {nr: 1, info: 'xxxx'} ];
	    $scope.reservationTyp = 0;
	}
    };

    $scope.names = [];
    var loadPlayers = function () {
	$http.get(url + "/get/persons").success(function (data) {
	    alert(data);
	    for (i=0; i<data.length; i++) {
		$scope.names.push(data[i].fname + " " + data[i].lname);
	    }
	})
    };

    $scope.gamelength = 1;

    $scope.setGameLength = function (cnt) {
	$scope.gamelength = cnt;
	var date = document.getElementById('datetimepicker2Value').value.split(' ')[0];
	var time = document.getElementById('datetimepicker2Value').value.split(' ')[1];
	$scope.triggerOverview(date, time);
    }

    $scope.triggerOverview = function (date, time) {
	var tmpCourts = [1,2,3,4];
	$scope.courts = [1,2,3,4];
	$scope.errorMsg = null;
	$scope.infoMsg = null;
	$http.get(url + "/get/resbydatetime?date=" + date + "&btime=" + time + "&length=" + $scope.gamelength).success(function (data) {
	    $scope.reservations = data;
	    if ($scope.reservations.length == 0) {
		$scope.reservationCourt = "1";
		$scope.courts = [1,2,3,4];
		if ($scope.currentLoginName != null) {
		    var l = (parseInt(moment(date + " " + time).format("HH")) +
			     parseInt($scope.gamelength));
		    switch($scope.currentLoginName.role) {
		    case 3:
			if (l > 17) {
			    $scope.courts = [3,4];
			    $scope.infoMsg = "Du kannst ab 17 Uhr nur mehr Platz 3 und Platz 4 reservieren.";
			    $scope.errorMsg = "Du hast leider keine Berechtigung nach 17 Uhr einen Platz 1 und Platz 2 zu reservieren!";
			}
			break;
		    case 4:
			if (l > 17) {
			    $scope.courts = [];
			    $scope.errorMsg = "Du hast leider keine Berechtigung nach 17 Uhr einen Platz zu reservieren!";
			}
			break;			
		    default:
			$scope.infoMsg = null;
			$scope.erroMsg = null;
			break;
		    }
		}
	    } else {
		for (i = 0; i < $scope.reservations.length; i++) {
		    var index = tmpCourts.indexOf($scope.reservations[i].court);
		    if (index >= 0) {
			tmpCourts.splice(index, 1);
		    }
		}
		$scope.reservationCourt = "" + tmpCourts[0];
		$scope.courts = tmpCourts;
		if ($scope.currentLoginName != null) {
		    var l = (parseInt(moment(date + " " + time).format("HH")) +
			     parseInt($scope.gamelength));
		    switch($scope.currentLoginName.role) {
		    case 3:
			if (l > 17) {
			    var xcourts = [];
			    if ($scope.courts.indexOf(3) >= 0) {
				xcourts.push(3);
			    }
			    if ($scope.courts.indexOf(4) >= 0) {
				xcourts.push(4);
			    }
			    $scope.courts = xcourts;
			    console.log($scope.courts);
			    $scope.infoMsg = "Du kannst ab 17 Uhr nur mehr Platz 3 und Platz 4 reservieren.";
			    $scope.errorMsg = "Du hast leider keine Berechtigung nach 17 Uhr einen Platz 1 und Platz 2 zu reservieren!";
			}
			break;
		    case 4:
			if (l > 17) {
			    $scope.courts = [];
			    $scope.errorMsg = "Du hast leider keine Berechtigung nach 17 Uhr einen Platz zu reservieren!";
			}
			break;			
		    default:
			$scope.infoMsg = null;
			$scope.erroMsg = null;
			break;
		    }
		}
	    }
	});
    };

    $scope.save = function () {
	var name1 = document.getElementById('player1');
	var name2 = document.getElementById('player2');
	var name3 = document.getElementById('player3');
	var name4 = document.getElementById('player4');

	if (name1 != null) { name1 = name1.value } else { name1 = "" };
	if (name2 != null) { name2 = name2.value } else { name2 = "" };
	if (name3 != null) { name3 = name3.value } else { name3 = "" };
	if (name4 != null) { name4 = name4.value } else { name4 = "" };

	var date = document.getElementById('datetimepicker2Value').value.split(' ')[0];
	var time = document.getElementById('datetimepicker2Value').value.split(' ')[1];
	var length = $scope.gamelength;
	var court = "";
	var courts = [];
	if ($scope.reservationCourt != undefined) {
	    court = $scope.reservationCourt;
	}

	var toggle1 = document.getElementById('courtSelect-1');
	var toggle2 = document.getElementById('courtSelect-2');
	var toggle3 = document.getElementById('courtSelect-3');
	var toggle4 = document.getElementById('courtSelect-4');	

	if (toggle1 != null) {
	    if (toggle1.checked) {
		courts.push(1);
	    }
	}
	if (toggle2 != null) {
	    if (toggle2.checked) {
		courts.push(2);
	    }
	}
	if (toggle3 != null) {
	    if (toggle3.checked) {
		courts.push(3);
	    }
	}
	if (toggle4 != null) {
	    if (toggle4.checked) {
		courts.push(4);
	    }
	}

	console.log("+++" + courts);

	function switchResult(a) {
	    switch (a) {
	    case 'Einzel':
		return 1;
		break;
	    case 'Doppel':
		return 2;
		break;
	    case 'Training':
		return 3;
		break;
	    case 'Meisterschaft':
		return 4;
		break;
	    case 'Ranglistenmatch':
		return 5;
		break;
	    default:
		return 4;
		break;
	    }
	}

	function getComment(c) {
	    if (c != null) {
		return c.value;
	    } else {
		return "";
	    }
	}

	var type = switchResult($scope.reservationTypSelect)
	var comment = getComment(document.getElementById('comment'));

	for (i=0; i<courts.length; i++) {
	    $.post( url + "/put/reservation",
		    { name1: name1,
		      name2: name2,
		      name3: name3,
		      name4: name4,
		      date: date,
		      time: time,
		      length: length,
		      court: courts[i],
		      type: type,
		      comment: comment
		    } ).fail( function (xhr, status, error) {
			window.location = url + "?status=1&msg=" + xhr.responseText;
		    }).done( function (data) {
			var msg = "Reservierung erfolgreich gespeichert!";
			window.location = url + "?status=0&msg=" + msg;
		    });
	}
	
    };
    
    $scope.currentLoginName = null;
    $scope.reservationNr = 0;
    var currentLogin = function () {
	return $http.get(url + "/auth/current").success(function (data) {
	    if (data != "") {
		$scope.currentLoginName = data;
		switch($scope.currentLoginName.role) {
		case 0: // super user
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			max: 10,
			postfix: "Stunde(n)"
		    });
		    $scope.items = ['Einzel',
				    'Doppel',
				    'Training',
				    'Meisterschaft',
				    'Ranglistenmatch'];
		    $scope.reservationNr = 500;
		    break;
		case 1:  // admin
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			max: 10,
			postfix: "Stunde(n)"
		    });
		    $scope.items = ['Einzel',
				    'Doppel',
				    'Training',
				    'Meisterschaft',
				    'Ranglistenmatch'];
		    $scope.reservationNr = 500;
		    break;
		case 2:  // member
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			max: 5,
			postfix: "Stunde(n)"
		    });
		    $scope.items = ['Einzel', 'Doppel'];
		    $scope.reservationNr = 2;
		    break;
		case 3:  // trainer
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			max: 10,
			postfix: "Stunde(n)"
		    });
		    $scope.items = ['Training'];
		    $scope.reservationNr = 500;
		    alert("trainer");
		    break;
		case 4:  // youth
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			max: 2,
			postfix: "Stunde(n)"
		    });
		    $scope.items = ['Einzel', 'Doppel'];
		    $scope.reservationNr = 2;
		    break;
		default:
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			max: 10,
			postfix: "Stunde(n)"
		    });
		    $scope.items = ['Einzel', 'Doppel', 'Training', 'Meisterschaft', 'Ranglistenmatch'];
		    $scope.reservationNr = 500;
		    break;
		}
	    }
	});
    };

    $scope.userReservations = [];
    var getUserResNr = function () {
	var nr = 0;	
	$http.get(url + "/get/resbyuser").success(function (data) {
	    var ret = data;
	    for (i=0; i<ret.length; i++) {
		var date = moment(ret[i].date + " " + ret[i].startTime);
		var now = moment(new Date());
		if (date > now) {
		    nr = nr + 1;
		    $scope.userReservations.push(ret[i]);
		}
	    }
	    alert($scope.reservationNr);
	    if ($scope.userReservations.length < $scope.reservationNr) {
		document.getElementById('saveForm').style.display = "block";
		document.getElementById('saveError').style.display = "none";
	    } else {
		document.getElementById('saveForm').style.display = "none";
		document.getElementById('saveError').style.display = "block";
	    }
	    return nr;
	});
    };

    $scope.deleteRes = function(resId) {
	$http.get(url + "/del/reservation?resId=" + resId).then(function successCallback(res) {
	    window.location = url + "/new.html";
	}, function errorCallback(res) {
	    window.location = url + "/new.html";
	});
    };

    $scope.freeReservations = 0;

    var init = function () {
	$scope.userReservations = [];
	var res_length = 0;
	return $http.get(url + "/auth/current").success(function (data) {
	    var nr = 0;	
	    $http.get(url + "/get/resbyuser").success(function (data) {
		var ret = data;
		for (i=0; i<ret.length; i++) {
		    var start = moment(ret[i].date + " " + ret[i].startTime);
		    var stop = moment(ret[i].date + " " + ret[i].stopTime);
		    var now = moment(new Date());
		    if (start > now) {
			nr = nr + 1;
			$scope.userReservations.push(ret[i]);
			res_length = res_length + stop.diff(start,'hours');
		    }
		}
//		$scope.freeReservations = $scope.freeReservations - res_length;
		if ($scope.userReservations.length < $scope.reservationNr) {
		    $http.get(url + "/get/persons").success(function (data) {
			for (i=0; i<data.length; i++) {
			    $scope.names.push(data[i].fname + " " + data[i].lname);
			}
		    });
		}
		switch($scope.currentLoginName.role) {
		case 0:
		    $scope.freeReservations = 1000 - res_length;
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			postfix: "Stunde(n)",
			max: 10,
		    });
		    break;
		case 1:
		    $scope.freeReservations = 500 - res_length;
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			postfix: "Stunde(n)",
			max: 10
		    });
		    break;
		case 2:
		    $scope.freeReservations = 2 - res_length;
		    var x = $scope.freeReservations;
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			postfix: "Stunde(n)",
			max: x
		    });
		    break;
		case 3:
		    $scope.freeReservations = 500 - res_length;
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			postfix: "Stunde(n)",
			max: 10
		    });
		    break;
		case 4:
		    $scope.freeReservations = 2 - res_length;
		    var x = 2 - $scope.freeReservations;
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			postfix: "Stunde(n)",
			max: x
		    });
		    break;
		default:
		    $("input[name='gamelength']").TouchSpin({
			initval: 1,
			min: 1,
			postfix: "Stunde(n)",
			max: 10
		    });
		    break;
		}
	    });	    
	    if (data != "") {
		$scope.currentLoginName = data;
		switch($scope.currentLoginName.role) {
		case 0: // super user
		    $scope.items = ['Einzel',
				    'Doppel',
				    'Training',
				    'Meisterschaft',
				    'Ranglistenmatch'];
		    $scope.reservationNr = 500;
		    break;
		case 1:  // admin
		    $scope.items = ['Einzel',
				    'Doppel',
				    'Training',
				    'Meisterschaft',
				    'Ranglistenmatch'];
		    $scope.reservationNr = 500;
		    break;
		case 2:  // member
		    $scope.items = ['Einzel', 'Doppel'];
		    $scope.reservationNr = 2;
		    break;
		case 3:  // trainer
		    $scope.items = ['Training'];
		    $scope.reservationNr = 500;
		    break;
		case 4:  // youth
		    $scope.items = ['Einzel', 'Doppel'];
		    $scope.reservationNr = 2;
		    break;
		default:
		    $scope.items = ['Einzel', 'Doppel', 'Training', 'Meisterschaft', 'Ranglistenmatch'];
		    $scope.reservationNr = 500;
		    break;
		}
	    }
	});
    }
// 		if ($scope.userReservations.length < $scope.reservationNr) {
// 		    document.getElementById('saveForm').style.display = "block";
// 		    $http.get(url + "/get/persons").success(function (data) {
// 			for (i=0; i<data.length; i++) {
// 			    $scope.names.push(data[i].fname + " " + data[i].lname);
// //			    document.getElementById('saveForm').style.display = "block";
// //			    document.getElementById('saveError').style.display = "none";
// 			}
// 		    })
// 		} else {
// //		    document.getElementById('saveForm').style.display = "none";
// 		    document.getElementById('saveError').style.display = "block";
// 		}
// 		return nr;
//	    });
//		    
//	});
//    };

    $scope.addUser = function () {
	document.getElementById('player1').value = "" + $scope.currentLoginName.fname + " " + $scope.currentLoginName.lname;
    };
    
    
    init();
    
//    currentLogin();
//    loadPlayers();
//    getUserResNr();

    var date = moment(new Date()).format("YYYY-MM-DD");
    var time = moment(new Date()).add(1,'hour').format("HH");

    $scope.triggerOverview(date, time);
    $scope.courtSelected = null;    

}]);

reservationApp.controller('LoginCtrl', ['$scope', '$http', function($scope, $http) {
    $scope.failure = getParameterByName("failure");
    $scope.rootDir = rootDir;
    $scope.gotoLoc = function (loc) {
	window.open(rootDir + loc, '_self');
    }
    $scope.login = function () {
	console.log(url + "/auth/login");
//	$http.get(url + "/auth/login").success(function (data) {
//	});
    };
    var loginForm = document.getElementById("loginform");
    var registerForm = document.getElementById("registerform");

    if (loginForm != null) { loginForm.action = rootDir + 'auth/login'; }
    if (registerForm != null) { registerForm.action = rootDir + 'auth/register'; }
			     
}]);

reservationApp.controller('StatCtrl', ['$scope', '$http', function($scope, $http) {
    $scope.currentLoginName = null;
    var currentLogin = function () {
	$http.get(url + "/auth/current").success(function (data) {
	    if (data != "") {
		$scope.currentLoginName = data;
	    }
	});
    };

    var getUserRes = function () {
	$http.get(url + "/get/resbyuser").success(function (data) {
	    $scope.userReservations = data;
	});
    };

    $scope.reservations = []
    $scope.mygames = [];
    $scope.persons = [];
    var getReservationsNr = function () {
	$http.get(url + "/get/reservations").success(function (data) {
	    $scope.reservations = data;
	    for (i=0; i<$scope.reservations.length; i++) {
		var persons = $scope.reservations[i].persons;
		for (j=0; j<persons.length; j++) {
		    if (persons[j].fname == $scope.currentLoginName.fname && persons[j].lname ==$scope.currentLoginName.lname) {
			$scope.mygames.push($scope.reservations[i]);
		    }
		}
	    }
	    $http.get(url + "/get/persons").success(function (data) {
		$scope.persons = data;
	    });
	})
    };

    currentLogin();
    getUserRes();
    getReservationsNr();
}]);

reservationApp.controller('HelpCtrl', ['$scope', '$http', function($scope, $http) {
    $scope.currentLoginName = null;
    var currentLogin = function () {
	$http.get(url + "/auth/current").success(function (data) {
	    if (data != "") {
		$scope.currentLoginName = data;
	    }
	});
    };

    currentLogin();    
}]);

reservationApp.controller('AdminCtrl', ['$scope', '$http', function($scope, $http) {
    $scope.rootDir = rootDir;
    $scope.users = null;
    var getUsers = function () {
	$http.get(url + "/get/users").success(function (data) {
	    $scope.users = data;
	});
    };

    $scope.roles = [
	{id: 0, name: 'SuperUser'},
	{id: 1, name: 'Admin'},
	{id: 2, name: 'Mitglied'},
	{id: 3, name: 'Trainer' },
	{id: 4, name: 'Jugendliche(r)' }
    ];

    $scope.reservations = null;
    var getReservations = function () {
	$http.get(url + "/get/reservations").success(function (data) {
	    $scope.reservations = data;
	});
    };

    $scope.editUser = {};
    $scope.modify = function (data) {
	$scope.editUser[data.id] = true;
	$scope.selectedRole = data.role;
    }

    $scope.edit = function (data) {
	$scope.editUser[data.id] = false;
	var currentUser = null;
	for (i=0; i<$scope.users.length; i++) {
	    if ($scope.users[i].id == data.id) {
		currentUser = $scope.users[i];
	    }
	}
	// only save if the role has changed
	if (currentUser.role != this.selectedRole) {
	    data.role = this.selectedRole;
	    $.post( url + "/put/user",
		    { id: currentUser.id,
		      role: this.selectedRole
		    }).fail( function (xhr, status, error) {
		    }).done( function (data) {
		    });
	}
    }

    $scope.currentLoginName = null;
    var currentLogin = function () {
	return $http.get(url + "/auth/current").success(function (data) {
	    if (data != "") {
		$scope.currentLoginName = data;
	    }
	});
    }

    currentLogin();
    getUsers();
    getReservations();
}]);

reservationApp.controller('PersonCtrl', ['$scope', '$http', function($scope, $http) {
    $scope.rootDir = rootDir;
    $scope.currentLoginName = null;
    var currentLogin = function () {
	return $http.get(url + "/auth/current").success(function (data) {
	    if (data != "") {
		$scope.currentLoginName = data;
	    }
	});
    }

    $scope.userReservations = [];
    var getUserRes = function () {
	$http.get(url + "/get/resbyuser").success(function (data) {
	    var ret = data;
	    for (i=0; i<ret.length; i++) {
		var date = moment(ret[i].date + " " + ret[i].startTime);
		var now = moment(new Date());
		if (date > now) {
		    $scope.userReservations.push(ret[i]);
		}
	    }
	});
    };

    $scope.deleteRes = function(resId) {
	$http.get(url + "/del/reservation?resId=" + resId).then(function successCallback(res) {
	    window.location = url + "/dash.html";
	}, function errorCallback(res) {
	    window.location = url + "/dash.html";
	});
    };
    
    currentLogin();
    getUserRes();
}]);
