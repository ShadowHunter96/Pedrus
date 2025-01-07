
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * Hodnoty statusCode a jim odpovídající
 *             hodnoty statusText:
 *             0 … OK
 *             1 … OK - příliš mnoho DIČ v dotazu.
 *             Vrácenou pouze prvních 100 subjektů.
 *             Tento status se vztahuje pouze na operaci getStatusNespolehlivyPlatce,
 *             operace getSeznamNespolehlivyPlatce vrací vždy všechny
 *             nespolehlivé plátce.
 *             2 … Technologická odstávka služby -
 *             0:00-0:10.
 *             3 … Služba nedostupná
 * 
 *             Atribut bezVypisuUctu urcuje, zda jsou v odpovědi vynechány seznamy účtů,
 *             pokud odpověď obsahuje seznamy účtů, pak není tento atribut
 *             v odpovědi uveden. Vatahuje se pouze k operaci
 *             getSeznamNespolehlivyPlatce.
 *           
 * 
 * <p>Java class for StatusType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="StatusType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;attribute name="odpovedGenerovana" use="required" type="{http://www.w3.org/2001/XMLSchema}date" /&gt;
 *       &lt;attribute name="statusCode" use="required" type="{http://www.w3.org/2001/XMLSchema}int" /&gt;
 *       &lt;attribute name="statusText" use="required" type="{http://www.w3.org/2001/XMLSchema}string" /&gt;
 *       &lt;attribute name="bezVypisuUctu" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}BezVypisuUctuType" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "StatusType")
public class StatusType {

    @XmlAttribute(name = "odpovedGenerovana", required = true)
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar odpovedGenerovana;
    @XmlAttribute(name = "statusCode", required = true)
    protected int statusCode;
    @XmlAttribute(name = "statusText", required = true)
    protected String statusText;
    @XmlAttribute(name = "bezVypisuUctu")
    protected BezVypisuUctuType bezVypisuUctu;

    /**
     * Gets the value of the odpovedGenerovana property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getOdpovedGenerovana() {
        return odpovedGenerovana;
    }

    /**
     * Sets the value of the odpovedGenerovana property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setOdpovedGenerovana(XMLGregorianCalendar value) {
        this.odpovedGenerovana = value;
    }

    /**
     * Gets the value of the statusCode property.
     * 
     */
    public int getStatusCode() {
        return statusCode;
    }

    /**
     * Sets the value of the statusCode property.
     * 
     */
    public void setStatusCode(int value) {
        this.statusCode = value;
    }

    /**
     * Gets the value of the statusText property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStatusText() {
        return statusText;
    }

    /**
     * Sets the value of the statusText property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStatusText(String value) {
        this.statusText = value;
    }

    /**
     * Gets the value of the bezVypisuUctu property.
     * 
     * @return
     *     possible object is
     *     {@link BezVypisuUctuType }
     *     
     */
    public BezVypisuUctuType getBezVypisuUctu() {
        return bezVypisuUctu;
    }

    /**
     * Sets the value of the bezVypisuUctu property.
     * 
     * @param value
     *     allowed object is
     *     {@link BezVypisuUctuType }
     *     
     */
    public void setBezVypisuUctu(BezVypisuUctuType value) {
        this.bezVypisuUctu = value;
    }

}
