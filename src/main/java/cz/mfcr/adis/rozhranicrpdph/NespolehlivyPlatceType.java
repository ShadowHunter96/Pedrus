
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for NespolehlivyPlatceType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <pre>
 * &lt;simpleType name="NespolehlivyPlatceType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="NE"/&gt;
 *     &lt;enumeration value="ANO"/&gt;
 *     &lt;enumeration value="NENALEZEN"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "NespolehlivyPlatceType")
@XmlEnum
public enum NespolehlivyPlatceType {

    NE,
    ANO,
    NENALEZEN;

    public String value() {
        return name();
    }

    public static NespolehlivyPlatceType fromValue(String v) {
        return valueOf(v);
    }

}
