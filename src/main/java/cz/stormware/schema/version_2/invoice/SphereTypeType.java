//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:43 PM CEST 
//


package cz.stormware.schema.version_2.invoice;

import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlEnumValue;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for sphereTypeType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <pre>
 * &lt;simpleType name="sphereTypeType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="business"/&gt;
 *     &lt;enumeration value="loan"/&gt;
 *     &lt;enumeration value="specificRegulations"/&gt;
 *     &lt;enumeration value="other"/&gt;
 *     &lt;enumeration value="cheques"/&gt;
 *     &lt;enumeration value="exchangeRateDifference"/&gt;
 *     &lt;enumeration value="penalty"/&gt;
 *     &lt;enumeration value="labourLaw"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "sphereTypeType")
@XmlEnum
public enum SphereTypeType {


    /**
     * Obchodní.
     * 
     */
    @XmlEnumValue("business")
    BUSINESS("business"),

    /**
     * Půjčka.
     * 
     */
    @XmlEnumValue("loan")
    LOAN("loan"),

    /**
     * Zvl. předpis.
     * 
     */
    @XmlEnumValue("specificRegulations")
    SPECIFIC_REGULATIONS("specificRegulations"),

    /**
     * Ostatní.
     * 
     */
    @XmlEnumValue("other")
    OTHER("other"),

    /**
     * Šeky.
     * 
     */
    @XmlEnumValue("cheques")
    CHEQUES("cheques"),

    /**
     * Kursové rozdíly.
     * 
     */
    @XmlEnumValue("exchangeRateDifference")
    EXCHANGE_RATE_DIFFERENCE("exchangeRateDifference"),

    /**
     * Penále.
     * 
     */
    @XmlEnumValue("penalty")
    PENALTY("penalty"),

    /**
     * Pracovněprávní.
     * 
     */
    @XmlEnumValue("labourLaw")
    LABOUR_LAW("labourLaw");
    private final String value;

    SphereTypeType(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static SphereTypeType fromValue(String v) {
        for (SphereTypeType c: SphereTypeType.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
