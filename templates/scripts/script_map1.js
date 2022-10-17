$(function () {
    $("#map1").attr('tabindex',-1);
    $("svg").attr('role', 'img');
    $("svg").attr('aria-label', 'map application');

	const sero_list = [...new Set(data1.features.map(({properties: { Serotype }}) => Serotype))];
	const species_list = [...new Set(data1.features.map(({properties: { Species }}) => Species))];
	sero_list.sort();
	species_list.sort();
	$.each(sero_list, function (i, item) {
		$('#filter-sero').append($('<option>', {
			value: item,
			text : item
		}));
	});
	$.each(species_list, function (i, item) {
		$('#filter-species').append($('<option>', {
			value: item,
			text : item
		}));
	});
	refreshLayer_map12();
});

// --------------- Map of geographic distribution ---------------
function onEachPoint_map1 (feature, layer){
	var popupContent = "<b>Disease: </b>" + setup.disease +
	"<br><b>Epi Event ID: </b>" + feature.properties.epi_event_id +
	"<br><b>Serotype: </b>" + feature.properties.Serotype +
	"<br><b>Species: </b>" + feature.properties.Species +
	"<br><b>Country: </b>" + feature.properties.country +
	"<br><b>Locality: </b>" + feature.properties.Location_name +
	"<br><b>Reported date: </b>" + feature.properties.date;
	layer.bindPopup(popupContent);
}

function getColorPolygon_map1(s){
	return s=='p' ? '#D6324B' :
	s=='a' ? '#92D050' :
	s=='s' ? '#FEE490' :
	s=='n' ? '#B4BED1' :
	s=='-' ? '#ffffff10' :
	'#ffffff10';
}

// set dates for dateslider 1-1
var bmin = new Date(2005, 0, 1);
var smin = new Date();
var smax = new Date();
smin.setDate(smin.getDate() - 730);
smax.setDate(smax.getDate() -30);

$("#dateslider1-1").dateRangeSlider({
	bounds: {
		min: bmin,
		max: new Date()
	},
	defaultValues: {
		min: smin,
		max: smax
	},
	step: {
		days: 1
	}
});

// set dates for dateslider 1-2
var smin_2 = new Date(2020, 6, 1);
var smax_2 = new Date(2021, 5, 31);

$("#dateslider1-2").dateRangeSlider({
	bounds: {
		min: bmin,
		max: new Date()
	},
	defaultValues: {
		min: smin_2,
		max: smax_2
	},
	step: {
		months: 6
	}
});

$("#dateslider1-1").bind("valuesChanged", function(e, data){
	smin = data.values.min;
	smax = data.values.max;
	refreshLayer_map11();
});

$("#dateslider1-2").bind("valuesChanged", function(e, data){
	smin_2 = data.values.min;
	smax_2 = data.values.max;
	refreshLayer_map12();
});

var opt_sero = $( '#filter-sero option:selected' ).val();
var opt_spc = $( '#filter-species option:selected' ).val();

$('#filter-sero').change(function(){
    opt_sero = $( '#filter-sero option:selected' ).val();
	refreshLayer_map11();
});
$('#filter-species').change(function(){
    opt_spc = $( '#filter-species option:selected' ).val();
	refreshLayer_map11();
});

var myRenderer = L.canvas({ padding: 0.1 });
var map1 = L.map('map1', {
	center: [45, 40],
	zoom: 2,
	minZoom: 2,
	maxBounds: L.latLngBounds(L.latLng(-90, -180), L.latLng(90, 180))
});

map1.createPane('pane11');
map1.createPane('pane12');
map1.getPane('pane12').style.zIndex = 200;
map1.getPane('pane11').style.zIndex = 300;

var streets1 = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}', {
	attribution: 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ &copy; <a href="https://data.europa.eu/">Joint Research Centre</a>',
	maxZoom: 16
});

var baseLayer1 = {};
var lyr_outbreaks_domestic = new L.featureGroup;
var lyr_outbreaks_wild = new L.featureGroup;
var overlays_map11_1= {
	"Domestic": lyr_outbreaks_domestic,
	"Wild": lyr_outbreaks_wild
};

function refreshLayer_map11() {
	lyr_outbreaks_domestic.clearLayers();
	lyr_outbreaks_wild.clearLayers();
	L.geoJson(data1, {
		filter: function (feature, layer) {
			var sampledate = new Date(feature.properties.date);
			if(opt_sero=="all" & opt_spc != "all")
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="false" &
				feature.properties.Species === opt_spc;
			else if(opt_sero!="all" & opt_spc=="all")
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="false" &
				feature.properties.Serotype === opt_sero;
			else if(opt_sero=="all" & opt_spc=="all")
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="false";
			else
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="false" &
				feature.properties.Serotype === opt_sero &
				feature.properties.Species === opt_spc;
		},
		onEachFeature: onEachPoint_map1,
		pointToLayer: function (feature, latlng) {
			return L.circleMarker(latlng, {
				renderer: myRenderer,
				radius: 4.5,
				fillColor: '#5c5c5c',
				color: 'black',
				weight: 1,
				opacity: 0.7,
				fillOpacity: 0.6,
				pane: 'pane11'
			});
		}
	}).addTo(lyr_outbreaks_domestic);

	L.geoJson(data1, {
		filter: function (feature, layer) {
			var sampledate = new Date(feature.properties.date);
			if(opt_sero=="all" & opt_spc != "all")
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="true" &
				feature.properties.Species === opt_spc;
			else if(opt_sero!="all" & opt_spc=="all")
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="true" &
				feature.properties.Serotype === opt_sero;
			else if(opt_sero=="all" & opt_spc=="all")
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="true";
			else
				return (sampledate >= smin && sampledate <= smax) &
				feature.properties.is_wild==="true" &
				feature.properties.Serotype === opt_sero &
				feature.properties.Species === opt_spc;
		},
		style: function (feature) {
			return feature.properties && feature.properties.style;
		},
		onEachFeature: onEachPoint_map1,
		pointToLayer: function (feature, latlng) {
			return L.circleMarker(latlng, {
				renderer: myRenderer,
				radius: 4.5,
				fillColor: '#0f55c5',
				color: 'black',
				weight: 1,
				opacity: 0.7,
				fillOpacity: 0.6,
				pane: 'pane11'
			});
		}
	}).addTo(lyr_outbreaks_wild);
}

// check number of AnimalCategory and divide into separate layers
const category = [...new Set(data1_semester.map(item => item.AnimalCategory))];
//pt_source.sort();

var overlays_map12_1= {};
for ( i in category) {
    this['layer_map12_'+i] = new L.featureGroup;
	overlays_map12_1[category[i]] = window['layer_map12_'+i];
}

function getUniqueArrBy(arr, key) {
    return [...new Map(arr.map(item => [item[key], item])).values()]
}

function refreshLayer_map12() {
	for ( i in category) {
		window['layer_map12_'+i].clearLayers();

		//filter semester info by date
		var dt_semester = data1_semester.filter(d => {
			var dateparts_f = d.FromDate.split('-');
			var f = new Date(dateparts_f[0], dateparts_f[1]-1, dateparts_f[2]);
			dateparts_t = d.ToDate.split('-');
			var t = new Date(dateparts_t[0], dateparts_t[1]-1, dateparts_t[2]);
			return (smin_2 <= f && t <= smax_2 && d.AnimalCategory == category[i]); });

		//reclass disease status
		var dt_semester_sum = [];
		for(j in dt_semester) {
			var cntr_list = [...new Set(dt_semester_sum.map(item => item.OfficialName))];

			if(!cntr_list.includes(dt_semester[j])){
				var status_list = [...new Set(dt_semester.map(function (item){
					if(item.OfficialName==dt_semester[j].OfficialName) return item.DiseaseStatus; }))];

				if(status_list.includes('p')){
					dt_semester_sum.push({OfficialName:dt_semester[j].OfficialName, DiseaseStatus:'p', AnimalCategory:dt_semester[j].AnimalCategory});
				}
				else {
					if(status_list.includes('s')){
						dt_semester_sum.push({OfficialName:dt_semester[j].OfficialName, DiseaseStatus:'s', AnimalCategory:dt_semester[j].AnimalCategory});
					}
					else {
						if(status_list.includes('a')){
							dt_semester_sum.push({OfficialName:dt_semester[j].OfficialName, DiseaseStatus:'a', AnimalCategory:dt_semester[j].AnimalCategory});
						}
						else{
							if(status_list.includes('n')){
								dt_semester_sum.push({OfficialName:dt_semester[j].OfficialName, DiseaseStatus:'n', AnimalCategory:dt_semester[j].AnimalCategory});
							}
							else{
								if(status_list.includes('-')){
									dt_semester_sum.push({OfficialName:dt_semester[j].OfficialName, DiseaseStatus:'-', AnimalCategory:dt_semester[j].AnimalCategory});
								}
							}
						}
					}
				}
			}
		}

		dt_semester_sum = getUniqueArrBy(dt_semester_sum, 'OfficialName');

		L.geoJson(country1, {
			filter: function (feature, layer) {
				var dt_cntr = [...new Set(dt_semester_sum.map(item => item.OfficialName))];
				return ( dt_cntr.includes(feature.properties.name0) )
			},
			style:function (feature) {
				var status = dt_semester_sum.filter(function (d) {
						if(d.OfficialName == feature.properties.name0) return d;});

        		return {
        			fillColor: getColorPolygon_map1(status[0].DiseaseStatus),
        			weight: 1.5,
        			opacity: 0.6,
        			color: 'gray',
        			fillOpacity: 0.7,
					pane: 'pane12'
        	  };
        	}
		}).addTo(window['layer_map12_'+i]);
	}
}

function rmMap1Control() {
	$('.mapControl').hide();
	swMap1Control.addTo(map1);
}

var swMap1Control = L.easyButton('fa fa-edit fa-2x',
	function (btn, map1) {
		  $('.mapControl').show();
		   map1.removeControl(swMap1Control);
		},
		{ position: 'topright'}
);

var layer1_1 = L.control.layers(baseLayer1, overlays_map11_1, {collapsed:true, position: 'bottomright'}).addTo(map1);
var htmlObject_1 = layer1_1.getContainer();
var a_1 = document.getElementById('layer-control1');
    function setParent(el, newParent){
    newParent.appendChild(el);
}
setParent(htmlObject_1, a_1);

var layer1_2 = L.control.layers(baseLayer1, overlays_map12_1, {collapsed:true, position: 'bottomright'}).addTo(map1);
var htmlObject_2 = layer1_2.getContainer();
var a_2 = document.getElementById('layer-control2');
    function setParent(el, newParent){
    newParent.appendChild(el);
}
setParent(htmlObject_2, a_2);

streets1.addTo(map1);
lyr_outbreaks_domestic.addTo(map1);
lyr_outbreaks_wild.addTo(map1);
layer_map12_0.addTo(map1);
map1.zoomControl.setPosition('topleft');

// legend
var map1Legend = L.control({position: 'bottomright'});
map1Legend.onAdd = function (map) {
	var div = L.DomUtil.create('div', 'info legend'),
	labels = ['<span class="lyName">Disease events</span>'];

	labels.push('<i class="circle" style="background: #5c5c5c"></i> Domestic');
	labels.push('<i class="circle" style="background: #0f55c5"></i> Wild');

	labels.push('<br><span class="lyName">Disease situation</span>');
	labels.push('<i style="background:' + getColorPolygon_map1('-') + '"></i> No report available or <br>&emsp;&emsp;&nbsp;no outbreaks were reported');
	labels.push('<i style="background:' + getColorPolygon_map1('n') + '"></i> No information provided');
	labels.push('<i style="background:' + getColorPolygon_map1('a') + '"></i> Absent');
	labels.push('<i style="background:' + getColorPolygon_map1('s') + '"></i> Suspected');
	labels.push('<i style="background:' + getColorPolygon_map1('p') + '"></i> Present');

	labels.push('<span class ="update"><br>Map updated: ' + setup.datetimestamp + '</span>');
	div.innerHTML = labels.join('<br>');
	return div;
}
map1Legend.addTo(map1);
