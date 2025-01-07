//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:43 PM CEST 
//


package cz.stormware.schema.version_2.invoice;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for intrastatType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="intrastatType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="countryOfDestination" type="{http://www.stormware.cz/schema/version_2/type.xsd}string2" minOccurs="0"/&gt;
 *         &lt;element name="countryOfDispatch" type="{http://www.stormware.cz/schema/version_2/type.xsd}string2" minOccurs="0"/&gt;
 *         &lt;element name="countryOfOrigin" type="{http://www.stormware.cz/schema/version_2/type.xsd}string2" minOccurs="0"/&gt;
 *         &lt;element name="region" type="{http://www.stormware.cz/schema/version_2/type.xsd}string2" minOccurs="0"/&gt;
 *         &lt;element name="transaction" type="{http://www.stormware.cz/schema/version_2/type.xsd}string3" minOccurs="0"/&gt;
 *         &lt;element name="specialTransaction" type="{http://www.stormware.cz/schema/version_2/type.xsd}string2" minOccurs="0"/&gt;
 *         &lt;element name="termsOfDelivery" type="{http://www.stormware.cz/schema/version_2/type.xsd}string3" minOccurs="0"/&gt;
 *         &lt;element name="modeOfTransport" type="{http://www.stormware.cz/schema/version_2/type.xsd}string1" minOccurs="0"/&gt;
 *         &lt;element name="shippingCosts" type="{http://www.stormware.cz/schema/version_2/type.xsd}currencyType" minOccurs="0"/&gt;
 *         &lt;element name="VATIdOfPartner" type="{http://www.stormware.cz/schema/version_2/type.xsd}dicType" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "intrastatType", propOrder = {

})
public class IntrastatType {

    protected String countryOfDestination;
    protected String countryOfDispatch;
    protected String countryOfOrigin;
    protected String region;
    protected String transaction;
    protected String specialTransaction;
    protected String termsOfDelivery;
    protected String modeOfTransport;
    protected Double shippingCosts;
    @XmlElement(name = "VATIdOfPartner")
    protected String vatIdOfPartner;

    /**
     * Gets the value of the countryOfDestination property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCountryOfDestination() {
        return countryOfDestination;
    }

    /**
     * Sets the value of the countryOfDestination property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCountryOfDestination(String value) {
        this.countryOfDestination = value;
    }

    /**
     * Gets the value of the countryOfDispatch property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCountryOfDispatch() {
        return countryOfDispatch;
    }

    /**
     * Sets the value of the countryOfDispatch property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCountryOfDispatch(String value) {
        this.countryOfDispatch = value;
    }

    /**
     * Gets the value of the countryOfOrigin property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCountryOfOrigin() {
        return countryOfOrigin;
    }

    /**
     * Sets the value of the countryOfOrigin property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCountryOfOrigin(String value) {
        this.countryOfOrigin = value;
    }

    /**
     * Gets the value of the region property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRegion() {
        return region;
    }

    /**
     * Sets the value of the region property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRegion(String value) {
        this.region = value;
    }

    /**
     * Gets the value of the transaction property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransaction() {
        return transaction;
    }

    /**
     * Sets the value of the transaction property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransaction(String value) {
        this.transaction = value;
    }

    /**
     * Gets the value of the specialTransaction property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSpecialTransaction() {
        return specialTransaction;
    }

    /**
     * Sets the value of the specialTransaction property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSpecialTransaction(String value) {
        this.specialTransaction = value;
    }

    /**
     * Gets the value of the termsOfDelivery property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTermsOfDelivery() {
        return termsOfDelivery;
    }

    /**
     * Sets the value of the termsOfDelivery property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTermsOfDelivery(String value) {
        this.termsOfDelivery = value;
    }

    /**
     * Gets the value of the modeOfTransport property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getModeOfTransport() {
        return modeOfTransport;
    }

    /**
     * Sets the value of the modeOfTransport property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setModeOfTransport(String value) {
        this.modeOfTransport = value;
    }

    /**
     * Gets the value of the shippingCosts property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getShippingCosts() {
        return shippingCosts;
    }

    /**
     * Sets the value of the shippingCosts property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setShippingCosts(Double value) {
        this.shippingCosts = value;
    }

    /**
     * Gets the value of the vatIdOfPartner property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVATIdOfPartner() {
        return vatIdOfPartner;
    }

    /**
     * Sets the value of the vatIdOfPartner property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVATIdOfPartner(String value) {
        this.vatIdOfPartner = value;
    }

}
