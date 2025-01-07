
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * 
 *             Strukturovaná adresa subjektu
 *             
 * 
 * <p>Java class for Adresa complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Adresa"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="uliceCislo" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="castObce" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="mesto" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="psc" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="stat" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Adresa", propOrder = {
    "uliceCislo",
    "castObce",
    "mesto",
    "psc",
    "stat"
})
public class Adresa {

    @XmlElement(required = true)
    protected String uliceCislo;
    protected String castObce;
    @XmlElement(required = true)
    protected String mesto;
    @XmlElement(required = true)
    protected String psc;
    @XmlElement(required = true)
    protected String stat;

    /**
     * Gets the value of the uliceCislo property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getUliceCislo() {
        return uliceCislo;
    }

    /**
     * Sets the value of the uliceCislo property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setUliceCislo(String value) {
        this.uliceCislo = value;
    }

    /**
     * Gets the value of the castObce property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCastObce() {
        return castObce;
    }

    /**
     * Sets the value of the castObce property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCastObce(String value) {
        this.castObce = value;
    }

    /**
     * Gets the value of the mesto property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMesto() {
        return mesto;
    }

    /**
     * Sets the value of the mesto property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMesto(String value) {
        this.mesto = value;
    }

    /**
     * Gets the value of the psc property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPsc() {
        return psc;
    }

    /**
     * Sets the value of the psc property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPsc(String value) {
        this.psc = value;
    }

    /**
     * Gets the value of the stat property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStat() {
        return stat;
    }

    /**
     * Sets the value of the stat property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStat(String value) {
        this.stat = value;
    }

}
