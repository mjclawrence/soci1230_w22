---
output: pdf_document
geometry: margin=1.0in
fontsize: 12pt
header-includes:
  - \usepackage{setspace}
  - \singlespacing 
  - \usepackage{nopageno} 
---

## SOCI 1230
## Winter Term 2022
## Datawrapper Exercise

Open **http://www.datawrapper.de**. Click on `Start creating`, `Create new`, and `New map`.

For this exercise, we'll make a symbol map based on US states. After making those selections, click the blue `Proceed` button which will be on the bottom of every window until we are done.

We'll import an existing dataset based on latitudes and longitudes. The `colleges_geo.csv` file is available on Canvas. Download it from Canvas and then upload it to Datawrapper. Match the `longitude` and `latitude` columns. The map should populate with points. We aren't adding any points, so click `Proceed` when you are done. 

Now we are ready to refine the map. You can choose the shape of symbols and scale their sizes by values of a variable. It is more common to change the symbol colors based on the values of a variable. Change the colors based on the `mr_kq5_pq1` variable.

Under Legend, add a caption and change the format to match your values. Open the advanced options for the legend and click the box to enable highlighting on hover.

Under Appearance, toggle to make the map zoomable.

Before clicking Proceed, scroll back to the top and open the Annotate tab. This is where you can give your map a title and description. It is good to add data sources and bylines too.

The map labels will add major cities. You can also add labels for other places.

The tooltips are the hover labels for each point. You can customize them with some simple html:

- \<b> and \</b> will bold the wrapped text

- \<i> and \</i> will italicize the wrapped text

- \<big> and \</big> will make the wrapped text bigger

- \<small> and \</small> will make the wrapped text smaller

- \<br> will force a line break

Check the layout tab to see if you need to add any of those options. It is rare to do so.

Click Proceed when you are done. Publishing your map will give you a link and other sharing options.

Now let's try a locator map...