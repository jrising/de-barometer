      google.load("visualization", "1", {packages:["treemap"]});
      google.setOnLoadCallback(drawChart);

      formatTextWrap = function(text, maxLineLength) {
	  const words = text.replace(/[\r\n]+/g, ' ').split(' ');
	  let lineLength = 0;
  
	  // use functional reduce, instead of for loop 
	  var lines = words.reduce((result, word) => {
	      if (lineLength + word.length >= maxLineLength) {
		  lineLength = word.length;
		  return result + `\n${word}`; // don't add spaces upfront
	      } else {
		  lineLength += word.length + (result ? 1 : 0);
		  return result ? result + ` ${word}` : `${word}`; // add space only when needed
	      }
	  }, '');
	  if (lines[0] == "\n")
	      lines = lines.substring(1);
	  return lines.split("\n");
      };

      // dispnames = { ... } and boxdata = [ ... ] defined outside
	  
      function drawChart() {
	  var data = google.visualization.arrayToDataTable(boxdata);
	  var container = document.getElementById('chart_div');
          tree = new google.visualization.TreeMap(container);

	  google.visualization.events.addListener(tree, 'ready',
						  function() {
						      var morelines = [];

						      // svg namespace
						      var svgNS = container.getElementsByTagName('svg')[0].namespaceURI;

						      Array.prototype.forEach.call(container.getElementsByTagName('text'), function(text) {
							  if (text.getAttribute('text-anchor') === 'middle') {
							     var rect = text.parentNode.getElementsByTagName('rect')[0];
							     // exclude top node
							      if (rect.getAttribute('fill') !== '#cccccc'&&text.textContent[0]=='['&&text.textContent.slice(-1)==']') {
								 var title = dispnames["I"+text.textContent.substring(1,text.textContent.length-1)] || "";
								  if (rect.getAttribute('height')<12){
								      text.textContent='…';
								      return;
								  }
								  if (rect.getAttribute('height')<24||rect.getAttribute('width')<50){
								     chars=rect.getAttribute('width')/6-2;
								     if (chars>0)
									 text.textContent = title.substring(0,chars)+'…';
								     else
									 text.textContent='…';
								     return;
								 }
								 var lines = formatTextWrap(title, rect.getAttribute('width') / 6);
								 text.textContent = lines[0];
								 text.setAttribute('y', parseFloat(text.getAttribute('y'))-Math.min((lines.length-1)*parseFloat(text.getAttribute('font-size'))/2,parseFloat(rect.getAttribute('height'))/2-parseFloat(text.getAttribute('font-size'))));
								 for (ii=1;ii<lines.length;ii++){
								     node = text.cloneNode(true);
								     if (ii<lines.length-1&&(ii+1)*parseFloat(node.getAttribute('font-size'))>parseFloat(rect.getAttribute('height'))/2){
									 lines[ii]+='…';
									 node.textContent=lines[ii];
									 node.setAttribute('y', parseFloat(text.getAttribute('y')) + ii*parseFloat(node.getAttribute('font-size')));
									 morelines.push([text, node]);
									 break;
								     }
								     node.textContent=lines[ii];
								     node.setAttribute('y', parseFloat(text.getAttribute('y')) + ii*parseFloat(node.getAttribute('font-size')));
								     morelines.push([text, node]);
								 }
							      } else if (text.textContent[0]=='['){
								  text.textContent='…';
							      }
							  }
						      });

						      morelines.forEach(function (text) {
							  text[0].parentNode.appendChild(text[1]);
						      });
						  });

	  google.visualization.events.addListener(tree, 'select',
						  function() {
						      var selection = tree.getSelection();
						      var row = selection[0].row;
						      var value = data.getValue(row, 0)
						      var selectize = $('#sector_select')[0].selectize;
						      if (!(value in selectize.options))
							  selectize.addOption({value: value, label: value});
						      selectize.addItem(value);
						      console.log(calculateTotalSize(data, data.getValue(row, 0)));
						  })

	  function showFullTooltip(row, size, value) {
	      return '<div style="background:#fd9; padding:10px; border-style:solid">' +
		  '<span style="font-family:Courier"><b>' + data.getValue(row, 0) +
		  '</b>, ' + data.getValue(row, 1) + '</span><br>' +
		  data.getColumnLabel(2) +
		  ' (total value of this cell and its children): ' + size + '<br>' +
		  calculateTotalSize(data, data.getValue(row, 0)) + '<br>' +
		  data.getColumnLabel(3) + ': ' + value + ' </div>';
	  }

	  function calculateTotalSize(data, locationName) {
              var totalSize = 0;
              for (var i = 0; i < data.getNumberOfRows(); i++) {
		  if (data.getValue(i, 0) === locationName) {
		      totalSize += data.getValue(i, 2);
		  } else if (data.getValue(i, 1) === locationName) {
		      totalSize += calculateTotalSize(data, data.getValue(i, 0));
		  }
              }
              return totalSize;
	  }
	  
        tree.draw(data, {
            highlightOnMouseOver: false,
            maxDepth: 2,
            maxPostDepth: 4,
            minColor: '#f00',
            midColor: '#ff0',
	    maxColor: '#0f0',
	    headerHeight: 15,
            fontColor: 'black',
	    showScale: true,
	    generateTooltip: showFullTooltip
        });
      }

