
import sbt._
import Keys._

object PiccoloBuild extends Build
{
    type Sett = Project.Setting[_]
    
    val standardSettings = Defaults.defaultSettings
    
    lazy val root = Project(
        id          = "Piccolo",
        base        = file("."),
        settings    = standardSettings ++ Seq[Sett](
            name                := "Piccolo",
            scalaVersion        := "2.9.1",
            scalacOptions       += "-deprecation",
            libraryDependencies ++= Seq(
                "org.scalatest" % "scalatest_2.9.1" % "1.6.1",
                "org.piccolo2d" % "piccolo2d-core" % "1.3.1",
                "org.apache.commons" % "commons-compress" % "1.3",
                "org.geotools" % "gt-main" % "2.7.4",
                "org.geotools" % "gt-coverage" % "2.7.4",
                "org.geotools" % "gt-geotiff" % "2.7.4",
                "org.geotools" % "gt-opengis" % "2.7.4",
                "org.geotools" % "gt-process" % "2.7.4",
                "xml-apis" % "xml-apis-xerces" % "2.7.1" from "http://download.osgeo.org/webdav/xml-apis/xml-apis-xerces/2.7.1/xml-apis-xerces-2.7.1.jar",
                "org.geotools" % "gt-xml" % "2.7.4",
                "org.geotools" % "gt-epsg-hsql" % "2.7.4"
            ),
            resolvers += MavenRepository("osgeo","http://download.osgeo.org/webdav/geotools"),
            ivyXML :=
              <dependencies>
                <exclude org="xml-apis" name="xml-apis-xerces" rev="2.7.1"/>
              </dependencies>
        )
    )
}

