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
 * Dodací adresa.
 * 
 * <p>Java class for shipToAddressType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="shipToAddressType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="actionType" type="{http://www.stormware.cz/schema/version_2/type.xsd}actionTypeItem" minOccurs="0"/&gt;
 *         &lt;element name="id" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType" minOccurs="0"/&gt;
 *         &lt;element name="extId" type="{http://www.stormware.cz/schema/version_2/type.xsd}string64" minOccurs="0"/&gt;
 *         &lt;element name="company" type="{http://www.stormware.cz/schema/version_2/type.xsd}stringCompany" minOccurs="0"/&gt;
 *         &lt;element name="division" type="{http://www.stormware.cz/schema/version_2/type.xsd}string32" minOccurs="0"/&gt;
 *         &lt;element name="name" type="{http://www.stormware.cz/schema/version_2/type.xsd}string64" minOccurs="0"/&gt;
 *         &lt;element name="city" type="{http://www.stormware.cz/schema/version_2/type.xsd}string45" minOccurs="0"/&gt;
 *         &lt;element name="street" type="{http://www.stormware.cz/schema/version_2/type.xsd}string64" minOccurs="0"/&gt;
 *         &lt;element name="zip" type="{http://www.stormware.cz/schema/version_2/type.xsd}string15" minOccurs="0"/&gt;
 *         &lt;element name="country" type="{http://www.stormware.cz/schema/version_2/type.xsd}refType" minOccurs="0"/&gt;
 *         &lt;element name="phone" type="{http://www.stormware.cz/schema/version_2/type.xsd}string40" minOccurs="0"/&gt;
 *         &lt;element name="email" type="{http://www.stormware.cz/schema/version_2/type.xsd}string98" minOccurs="0"/&gt;
 *         &lt;element name="defaultShipAddress" type="{http://www.stormware.cz/schema/version_2/type.xsd}boolean" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "shipToAddressType", propOrder = {

})
public class ShipToAddressType {

    protected ActionTypeItem actionType;
    protected BigInteger id;
    protected String extId;
    protected String company;
    protected String division;
    protected String name;
    protected String city;
    protected String street;
    protected String zip;
    protected RefType country;
    protected String phone;
    protected String email;
    @XmlElement(defaultValue = "false")
    @XmlSchemaType(name = "string")
    protected Boolean defaultShipAddress;

    /**
     * Gets the value of the actionType property.
     * 
     * @return
     *     possible object is
     *     {@link ActionTypeItem }
     *     
     */
    public ActionTypeItem getActionType() {
        return actionType;
    }

    /**
     * Sets the value of the actionType property.
     * 
     * @param value
     *     allowed object is
     *     {@link ActionTypeItem }
     *     
     */
    public void setActionType(ActionTypeItem value) {
        this.actionType = value;
    }

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
     * Gets the value of the extId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getExtId() {
        return extId;
    }

    /**
     * Sets the value of the extId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setExtId(String value) {
        this.extId = value;
    }

    /**
     * Gets the value of the company property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCompany() {
        return company;
    }

    /**
     * Sets the value of the company property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCompany(String value) {
        this.company = value;
    }

    /**
     * Gets the value of the division property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDivision() {
        return division;
    }

    /**
     * Sets the value of the division property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDivision(String value) {
        this.division = value;
    }

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the city property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCity() {
        return city;
    }

    /**
     * Sets the value of the city property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCity(String value) {
        this.city = value;
    }

    /**
     * Gets the value of the street property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStreet() {
        return street;
    }

    /**
     * Sets the value of the street property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStreet(String value) {
        this.street = value;
    }

    /**
     * Gets the value of the zip property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getZip() {
        return zip;
    }

    /**
     * Sets the value of the zip property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setZip(String value) {
        this.zip = value;
    }

    /**
     * Gets the value of the country property.
     * 
     * @return
     *     possible object is
     *     {@link RefType }
     *     
     */
    public RefType getCountry() {
        return country;
    }

    /**
     * Sets the value of the country property.
     * 
     * @param value
     *     allowed object is
     *     {@link RefType }
     *     
     */
    public void setCountry(RefType value) {
        this.country = value;
    }

    /**
     * Gets the value of the phone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Sets the value of the phone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPhone(String value) {
        this.phone = value;
    }

    /**
     * Gets the value of the email property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEmail() {
        return email;
    }

    /**
     * Sets the value of the email property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEmail(String value) {
        this.email = value;
    }

    /**
     * Gets the value of the defaultShipAddress property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean getDefaultShipAddress() {
        return defaultShipAddress;
    }

    /**
     * Sets the value of the defaultShipAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setDefaultShipAddress(Boolean value) {
        this.defaultShipAddress = value;
    }

}
