//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.type;

import java.math.BigInteger;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for roundingItemType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="roundingItemType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="id" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType" minOccurs="0"/&gt;
 *         &lt;element name="text" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="quantity" type="{http://www.w3.org/2001/XMLSchema}float"/&gt;
 *         &lt;element name="payVAT" type="{http://www.stormware.cz/schema/version_2/type.xsd}boolean"/&gt;
 *         &lt;element name="rateVAT" type="{http://www.stormware.cz/schema/version_2/type.xsd}vatRateType"/&gt;
 *         &lt;element name="percentVAT" type="{http://www.w3.org/2001/XMLSchema}float" minOccurs="0"/&gt;
 *         &lt;element name="homeCurrency" type="{http://www.stormware.cz/schema/version_2/type.xsd}typeCurrencyHomeItemRounding" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "roundingItemType", propOrder = {

})
public class RoundingItemType {

    protected BigInteger id;
    @XmlElement(required = true)
    protected String text;
    protected float quantity;
    @XmlElement(required = true)
    @XmlSchemaType(name = "string")
    protected Boolean payVAT;
    @XmlElement(required = true)
    @XmlSchemaType(name = "string")
    protected VatRateType rateVAT;
    protected Float percentVAT;
    protected TypeCurrencyHomeItemRounding homeCurrency;

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setId(BigInteger value) {
        this.id = value;
    }

    /**
     * Gets the value of the text property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getText() {
        return text;
    }

    /**
     * Sets the value of the text property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setText(String value) {
        this.text = value;
    }

    /**
     * Gets the value of the quantity property.
     * 
     */
    public float getQuantity() {
        return quantity;
    }

    /**
     * Sets the value of the quantity property.
     * 
     */
    public void setQuantity(float value) {
        this.quantity = value;
    }

    /**
     * Gets the value of the payVAT property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean getPayVAT() {
        return payVAT;
    }

    /**
     * Sets the value of the payVAT property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPayVAT(Boolean value) {
        this.payVAT = value;
    }

    /**
     * Gets the value of the rateVAT property.
     * 
     * @return
     *     possible object is
     *     {@link VatRateType }
     *     
     */
    public VatRateType getRateVAT() {
        return rateVAT;
    }

    /**
     * Sets the value of the rateVAT property.
     * 
     * @param value
     *     allowed object is
     *     {@link VatRateType }
     *     
     */
    public void setRateVAT(VatRateType value) {
        this.rateVAT = value;
    }

    /**
     * Gets the value of the percentVAT property.
     * 
     * @return
     *     possible object is
     *     {@link Float }
     *     
     */
    public Float getPercentVAT() {
        return percentVAT;
    }

    /**
     * Sets the value of the percentVAT property.
     * 
     * @param value
     *     allowed object is
     *     {@link Float }
     *     
     */
    public void setPercentVAT(Float value) {
        this.percentVAT = value;
    }

    /**
     * Gets the value of the homeCurrency property.
     * 
     * @return
     *     possible object is
     *     {@link TypeCurrencyHomeItemRounding }
     *     
     */
    public TypeCurrencyHomeItemRounding getHomeCurrency() {
        return homeCurrency;
    }

    /**
     * Sets the value of the homeCurrency property.
     * 
     * @param value
     *     allowed object is
     *     {@link TypeCurrencyHomeItemRounding }
     *     
     */
    public void setHomeCurrency(TypeCurrencyHomeItemRounding value) {
        this.homeCurrency = value;
    }

}
