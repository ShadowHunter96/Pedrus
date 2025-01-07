
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
 *         &lt;element name="statusPlatceDPH" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}InformaceOPlatciType" maxOccurs="unbounded" minOccurs="0"/&gt;
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
    "statusPlatceDPH"
})
@XmlRootElement(name = "SeznamNespolehlivyPlatceResponse")
public class SeznamNespolehlivyPlatceResponse {

    @XmlElement(required = true)
    protected StatusType status;
    protected List<InformaceOPlatciType> statusPlatceDPH;

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
     * Gets the value of the statusPlatceDPH property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the statusPlatceDPH property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getStatusPlatceDPH().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link InformaceOPlatciType }
     * 
     * 
     */
    public List<InformaceOPlatciType> getStatusPlatceDPH() {
        if (statusPlatceDPH == null) {
            statusPlatceDPH = new ArrayList<InformaceOPlatciType>();
        }
        return this.statusPlatceDPH;
    }

}
