
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for StandardniUcetType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="StandardniUcetType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;attribute name="predcisli"&gt;
 *         &lt;simpleType&gt;
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *             &lt;pattern value="\d{1,6}"/&gt;
 *           &lt;/restriction&gt;
 *         &lt;/simpleType&gt;
 *       &lt;/attribute&gt;
 *       &lt;attribute name="cislo" use="required"&gt;
 *         &lt;simpleType&gt;
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *             &lt;pattern value="\d{1,10}"/&gt;
 *           &lt;/restriction&gt;
 *         &lt;/simpleType&gt;
 *       &lt;/attribute&gt;
 *       &lt;attribute name="kodBanky" use="required"&gt;
 *         &lt;simpleType&gt;
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *             &lt;pattern value="\d{4}"/&gt;
 *           &lt;/restriction&gt;
 *         &lt;/simpleType&gt;
 *       &lt;/attribute&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "StandardniUcetType")
public class StandardniUcetType {

    @XmlAttribute(name = "predcisli")
    protected String predcisli;
    @XmlAttribute(name = "cislo", required = true)
    protected String cislo;
    @XmlAttribute(name = "kodBanky", required = true)
    protected String kodBanky;

    /**
     * Gets the value of the predcisli property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPredcisli() {
        return predcisli;
    }

    /**
     * Sets the value of the predcisli property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPredcisli(String value) {
        this.predcisli = value;
    }

    /**
     * Gets the value of the cislo property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCislo() {
        return cislo;
    }

    /**
     * Sets the value of the cislo property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCislo(String value) {
        this.cislo = value;
    }

    /**
     * Gets the value of the kodBanky property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getKodBanky() {
        return kodBanky;
    }

    /**
     * Sets the value of the kodBanky property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setKodBanky(String value) {
        this.kodBanky = value;
    }

}
