
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for TypSubjektuDPHType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <pre>
 * &lt;simpleType name="TypSubjektuDPHType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="PLATCE_DPH"/&gt;
 *     &lt;enumeration value="IDENTIFIKOVANA_OSOBA"/&gt;
 *     &lt;enumeration value="NENALEZEN"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "TypSubjektuDPHType")
@XmlEnum
public enum TypSubjektuDPHType {

    PLATCE_DPH,
    IDENTIFIKOVANA_OSOBA,
    NENALEZEN;

    public String value() {
        return name();
    }

    public static TypSubjektuDPHType fromValue(String v) {
        return valueOf(v);
    }

}
