
package cz.mfcr.adis.rozhranicrpdph;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SeznamZverejnenychUctuType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SeznamZverejnenychUctuType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="ucet" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}ZverejnenyUcetType" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SeznamZverejnenychUctuType", propOrder = {
    "ucet"
})
public class SeznamZverejnenychUctuType {

    @XmlElement(required = true)
    protected List<ZverejnenyUcetType> ucet;

    /**
     * Gets the value of the ucet property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the ucet property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getUcet().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ZverejnenyUcetType }
     * 
     * 
     */
    public List<ZverejnenyUcetType> getUcet() {
        if (ucet == null) {
            ucet = new ArrayList<ZverejnenyUcetType>();
        }
        return this.ucet;
    }

}
