//setup style
$(function () {
	document.title = setup.title;
	$('#header2').text(setup.title);
	$( ".section" ).css( "color", "#ccc" );

    var itemSelected = "#ls-"+ tab2Selected +"> a";
    $(itemSelected).addClass('selected');
	$(itemSelected).parent().addClass('selected');
});

var tab2Selected = 1;

function formatDate(date) {
    var d = new Date(date),
        month = '' + (d.getMonth() + 1),
        day = '' + d.getDate(),
        year = d.getFullYear();

    if (month.length < 2)
        month = '0' + month;
    if (day.length < 2)
        day = '0' + day;

    return [year, month, day].join('-');
}

var lastClicked;
function highlightFeature(e) {
	var layer = e.target;
	if(lastClicked){
	   lastClicked.setStyle({
			weight: 1,
			opacity: 0.2,
			color: 'gray',
			fillOpacity: 0.5
		});
	}
	layer.setStyle({
		weight: 2,
		opacity: 1,
		color: "#27D8DD",
		fillOpacity: 0.7
	});
	lastClicked = layer;
}

function resetHighlight (e) {
	var layer = e.target;
	layer.setStyle({
		weight: 1,
		opacity: 0.2,
		color: 'gray',
		fillOpacity: 0.5
	});
}

function mapResize() {
	if (typeof map1 !== 'undefined')
		setTimeout(function () { map1.invalidateSize() }, 0);

	if (typeof map2 !== 'undefined')
		setTimeout(function () { map2.invalidateSize() }, 0);
}
// --------------- page control ---------------
$(document).ready(function() {
    var slideNum = $('.page').length,
    wrapperWidth = 100 * slideNum,
    slideWidth = 100 / slideNum;
    $('.wrapper').width(wrapperWidth + '%');
    $('.page').width(slideWidth + '%');

    mapResize();

    $('a.scrollitem').click(function() {
        $('a.scrollitem').removeClass('selected');
		$('li').removeClass('selected');
        $(this).addClass('selected');
		$(this).parent().addClass('selected');
        tab2Selected  = parseInt($(this).parent().attr('id').split('-')[1]);
        chkSlidePos(tab2Selected);

        mapResize();

        var slideNumber = $($(this).attr('href')).index('.page'), margin = slideNumber * -100 + '%';

        $('.wrapper').animate({
            marginLeft: margin
        }, 1500);
        return false;
    });
});

function slideRight (){
    if(tab2Selected < setup.numTabs){
        $('a.scrollitem').removeClass('selected');
		$('li').removeClass('selected');

        tab2Selected += 1;
        var itemSelected = "#ls-"+ tab2Selected +"> a";
        $(itemSelected).addClass('selected');
		$(itemSelected).parent().addClass('selected');

        mapResize();

        var slideNumber = $($(itemSelected).attr('href')).index('.page'), margin = slideNumber * -100 + '%';

        $('.wrapper').animate({
            marginLeft: margin
        }, 1500);
    }

	chkSlidePos(tab2Selected);
}

function slideLeft (){
    if(tab2Selected > 1){
        $('a.scrollitem').removeClass('selected');
		$('li').removeClass('selected');

        tab2Selected -= 1;
        var itemSelected = "#ls-"+ tab2Selected +"> a";
        $(itemSelected).addClass('selected');
		$(itemSelected).parent().addClass('selected');

        mapResize();

        var slideNumber = $($(itemSelected).attr('href')).index('.page'), margin = slideNumber * -100 + '%';

        $('.wrapper').animate({
            marginLeft: margin
         }, 1500);
    }
	chkSlidePos(tab2Selected);
}

function slideRef(ref){
    tab2Selected = setup.ref_id;
    $('a.scrollitem').removeClass('selected');
	$('li').removeClass('selected');

    var itemSelected = "#ls-"+setup.ref_id+" > a";

    mapResize();

    var slideNumber = $($(itemSelected).attr('href')).index('.page'), margin = slideNumber * -100 + '%';

    $('.wrapper').animate({
        marginLeft: margin
    }, 500);

	var refSelected = "#"+ref;
	$('a.ref').removeClass('refSelected');
	$(refSelected).find('a.ref').addClass('refSelected');

    var ref_elmnt = document.getElementById(ref);
	var topPos = ref_elmnt.offsetTop;
	var leftContent_id = 'leftContent-'+setup.ref_id;
	document.getElementById(leftContent_id).scrollTop = topPos;
}

function chkSlidePos(n) {
	if(n==1)
        $('.left').addClass('first');
    else
        $('.left').removeClass('first');

    if(n==setup.numTabs) {
    	$('a.ref').removeClass('refSelected');

        $('.right').addClass('last');
	}
	else{
	    $('.right').removeClass('last');
	}
}
