
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * Informace o plátci rozšířené o název a adresu subjektu
 * 
 * <p>Java class for InformaceOPlatciRozsireneType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InformaceOPlatciRozsireneType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://adis.mfcr.cz/rozhraniCRPDPH/}InformaceOPlatciType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="nazevSubjektu" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="adresa" type="{http://adis.mfcr.cz/rozhraniCRPDPH/}Adresa" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InformaceOPlatciRozsireneType", propOrder = {
    "nazevSubjektu",
    "adresa"
})
public class InformaceOPlatciRozsireneType
    extends InformaceOPlatciType
{

    protected String nazevSubjektu;
    protected Adresa adresa;

    /**
     * Gets the value of the nazevSubjektu property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNazevSubjektu() {
        return nazevSubjektu;
    }

    /**
     * Sets the value of the nazevSubjektu property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNazevSubjektu(String value) {
        this.nazevSubjektu = value;
    }

    /**
     * Gets the value of the adresa property.
     * 
     * @return
     *     possible object is
     *     {@link Adresa }
     *     
     */
    public Adresa getAdresa() {
        return adresa;
    }

    /**
     * Sets the value of the adresa property.
     * 
     * @param value
     *     allowed object is
     *     {@link Adresa }
     *     
     */
    public void setAdresa(Adresa value) {
        this.adresa = value;
    }

}
