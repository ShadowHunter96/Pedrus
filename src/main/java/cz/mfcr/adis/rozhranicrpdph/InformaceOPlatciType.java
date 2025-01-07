
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * <p>Java class for InformaceOPlatciType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InformaceOPlatciType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="zverejneneUcty" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}SeznamZverejnenychUctuType" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="dic" use="required" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}DICType" /&gt;
 *       &lt;attribute name="nespolehlivyPlatce" use="required" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}NespolehlivyPlatceType" /&gt;
 *       &lt;attribute name="datumZverejneniNespolehlivosti" type="{http://www.w3.org/2001/XMLSchema}date" /&gt;
 *       &lt;attribute name="cisloFu"&gt;
 *         &lt;simpleType&gt;
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *             &lt;pattern value="\d{2,3}"/&gt;
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
@XmlType(name = "InformaceOPlatciType", propOrder = {
    "zverejneneUcty"
})
@XmlSeeAlso({
    InformaceOPlatciRozsireneType.class,
    InformaceOSubjektuRozsireneType.class
})
public class InformaceOPlatciType {

    protected SeznamZverejnenychUctuType zverejneneUcty;
    @XmlAttribute(name = "dic", required = true)
    protected String dic;
    @XmlAttribute(name = "nespolehlivyPlatce", required = true)
    protected NespolehlivyPlatceType nespolehlivyPlatce;
    @XmlAttribute(name = "datumZverejneniNespolehlivosti")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar datumZverejneniNespolehlivosti;
    @XmlAttribute(name = "cisloFu")
    protected String cisloFu;

    /**
     * Gets the value of the zverejneneUcty property.
     * 
     * @return
     *     possible object is
     *     {@link SeznamZverejnenychUctuType }
     *     
     */
    public SeznamZverejnenychUctuType getZverejneneUcty() {
        return zverejneneUcty;
    }

    /**
     * Sets the value of the zverejneneUcty property.
     * 
     * @param value
     *     allowed object is
     *     {@link SeznamZverejnenychUctuType }
     *     
     */
    public void setZverejneneUcty(SeznamZverejnenychUctuType value) {
        this.zverejneneUcty = value;
    }

    /**
     * Gets the value of the dic property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDic() {
        return dic;
    }

    /**
     * Sets the value of the dic property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDic(String value) {
        this.dic = value;
    }

    /**
     * Gets the value of the nespolehlivyPlatce property.
     * 
     * @return
     *     possible object is
     *     {@link NespolehlivyPlatceType }
     *     
     */
    public NespolehlivyPlatceType getNespolehlivyPlatce() {
        return nespolehlivyPlatce;
    }

    /**
     * Sets the value of the nespolehlivyPlatce property.
     * 
     * @param value
     *     allowed object is
     *     {@link NespolehlivyPlatceType }
     *     
     */
    public void setNespolehlivyPlatce(NespolehlivyPlatceType value) {
        this.nespolehlivyPlatce = value;
    }

    /**
     * Gets the value of the datumZverejneniNespolehlivosti property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDatumZverejneniNespolehlivosti() {
        return datumZverejneniNespolehlivosti;
    }

    /**
     * Sets the value of the datumZverejneniNespolehlivosti property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDatumZverejneniNespolehlivosti(XMLGregorianCalendar value) {
        this.datumZverejneniNespolehlivosti = value;
    }

    /**
     * Gets the value of the cisloFu property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCisloFu() {
        return cisloFu;
    }

    /**
     * Sets the value of the cisloFu property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCisloFu(String value) {
        this.cisloFu = value;
    }

}
