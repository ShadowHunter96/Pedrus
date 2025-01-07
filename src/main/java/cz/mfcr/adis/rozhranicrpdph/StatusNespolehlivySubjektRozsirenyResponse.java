
package cz.mfcr.adis.rozhranicrpdph;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="status" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}StatusType"/&gt;
 *         &lt;element name="statusSubjektu" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}InformaceOSubjektuRozsireneType" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "status",
    "statusSubjektu"
})
@XmlRootElement(name = "StatusNespolehlivySubjektRozsirenyResponse")
public class StatusNespolehlivySubjektRozsirenyResponse {

    @XmlElement(required = true)
    protected StatusType status;
    protected List<InformaceOSubjektuRozsireneType> statusSubjektu;

    /**
     * Gets the value of the status property.
     * 
     * @return
     *     possible object is
     *     {@link StatusType }
     *     
     */
    public StatusType getStatus() {
        return status;
    }

    /**
     * Sets the value of the status property.
     * 
     * @param value
     *     allowed object is
     *     {@link StatusType }
     *     
     */
    public void setStatus(StatusType value) {
        this.status = value;
    }

    /**
     * Gets the value of the statusSubjektu property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the statusSubjektu property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getStatusSubjektu().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link InformaceOSubjektuRozsireneType }
     * 
     * 
     */
    public List<InformaceOSubjektuRozsireneType> getStatusSubjektu() {
        if (statusSubjektu == null) {
            statusSubjektu = new ArrayList<InformaceOSubjektuRozsireneType>();
        }
        return this.statusSubjektu;
    }

}
