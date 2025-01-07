
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * <p>Java class for ZverejnenyUcetType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ZverejnenyUcetType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;choice&gt;
 *           &lt;element name="standardniUcet" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}StandardniUcetType"/&gt;
 *           &lt;element name="nestandardniUcet" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}NestandardniUcetType"/&gt;
 *         &lt;/choice&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="datumZverejneni" use="required" type="{http://www.w3.org/2001/XMLSchema}date" /&gt;
 *       &lt;attribute name="datumZverejneniUkonceni" type="{http://www.w3.org/2001/XMLSchema}date" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ZverejnenyUcetType", propOrder = {
    "standardniUcet",
    "nestandardniUcet"
})
public class ZverejnenyUcetType {

    protected StandardniUcetType standardniUcet;
    protected NestandardniUcetType nestandardniUcet;
    @XmlAttribute(name = "datumZverejneni", required = true)
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar datumZverejneni;
    @XmlAttribute(name = "datumZverejneniUkonceni")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar datumZverejneniUkonceni;

    /**
     * Gets the value of the standardniUcet property.
     * 
     * @return
     *     possible object is
     *     {@link StandardniUcetType }
     *     
     */
    public StandardniUcetType getStandardniUcet() {
        return standardniUcet;
    }

    /**
     * Sets the value of the standardniUcet property.
     * 
     * @param value
     *     allowed object is
     *     {@link StandardniUcetType }
     *     
     */
    public void setStandardniUcet(StandardniUcetType value) {
        this.standardniUcet = value;
    }

    /**
     * Gets the value of the nestandardniUcet property.
     * 
     * @return
     *     possible object is
     *     {@link NestandardniUcetType }
     *     
     */
    public NestandardniUcetType getNestandardniUcet() {
        return nestandardniUcet;
    }

    /**
     * Sets the value of the nestandardniUcet property.
     * 
     * @param value
     *     allowed object is
     *     {@link NestandardniUcetType }
     *     
     */
    public void setNestandardniUcet(NestandardniUcetType value) {
        this.nestandardniUcet = value;
    }

    /**
     * Gets the value of the datumZverejneni property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDatumZverejneni() {
        return datumZverejneni;
    }

    /**
     * Sets the value of the datumZverejneni property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDatumZverejneni(XMLGregorianCalendar value) {
        this.datumZverejneni = value;
    }

    /**
     * Gets the value of the datumZverejneniUkonceni property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDatumZverejneniUkonceni() {
        return datumZverejneniUkonceni;
    }

    /**
     * Sets the value of the datumZverejneniUkonceni property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDatumZverejneniUkonceni(XMLGregorianCalendar value) {
        this.datumZverejneniUkonceni = value;
    }

}
