#get marks

marks_xml <- read_xml("./data/BYCMACK_Rhumbline.gpx")

xml_find_all(xml_children(xml_child(marks_xml)), "//rtept")

x <- read_xml("<body>
  <p>Some <b>text</b>.</p>
              <p>Some <b>other</b> <b>text</b>.</p>
              <rte>No bold here!</rte>
              </body>")
para <- xml_find_all(x, ".//rte")
