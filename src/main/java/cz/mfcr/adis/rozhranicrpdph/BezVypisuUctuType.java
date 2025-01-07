
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for BezVypisuUctuType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <pre>
 * &lt;simpleType name="BezVypisuUctuType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="ANO"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "BezVypisuUctuType")
@XmlEnum
public enum BezVypisuUctuType {

    ANO;

    public String value() {
        return name();
    }

    public static BezVypisuUctuType fromValue(String v) {
        return valueOf(v);
    }

}
