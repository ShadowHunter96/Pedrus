//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.filter;

import java.math.BigInteger;
import javax.xml.datatype.XMLGregorianCalendar;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for filterDocsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="filterDocsType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="id" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType" minOccurs="0"/&gt;
 *         &lt;element name="dateFrom" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *         &lt;element name="dateTill" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *         &lt;element name="selectedNumbers" type="{http://www.stormware.cz/schema/version_2/filter.xsd}selectedNumbersType" minOccurs="0"/&gt;
 *         &lt;element name="selectedCompanys" type="{http://www.stormware.cz/schema/version_2/filter.xsd}selectedCompanysType" minOccurs="0"/&gt;
 *         &lt;element name="selectedIco" type="{http://www.stormware.cz/schema/version_2/filter.xsd}selectedIcoType" minOccurs="0"/&gt;
 *         &lt;element name="lastChanges" type="{http://www.w3.org/2001/XMLSchema}dateTime" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "filterDocsType", propOrder = {

})
public class FilterDocsType {

    protected BigInteger id;
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar dateFrom;
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar dateTill;
    protected SelectedNumbersType selectedNumbers;
    protected SelectedCompanysType selectedCompanys;
    protected SelectedIcoType selectedIco;
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar lastChanges;

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
     * Gets the value of the dateFrom property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDateFrom() {
        return dateFrom;
    }

    /**
     * Sets the value of the dateFrom property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDateFrom(XMLGregorianCalendar value) {
        this.dateFrom = value;
    }

    /**
     * Gets the value of the dateTill property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDateTill() {
        return dateTill;
    }

    /**
     * Sets the value of the dateTill property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDateTill(XMLGregorianCalendar value) {
        this.dateTill = value;
    }

    /**
     * Gets the value of the selectedNumbers property.
     * 
     * @return
     *     possible object is
     *     {@link SelectedNumbersType }
     *     
     */
    public SelectedNumbersType getSelectedNumbers() {
        return selectedNumbers;
    }

    /**
     * Sets the value of the selectedNumbers property.
     * 
     * @param value
     *     allowed object is
     *     {@link SelectedNumbersType }
     *     
     */
    public void setSelectedNumbers(SelectedNumbersType value) {
        this.selectedNumbers = value;
    }

    /**
     * Gets the value of the selectedCompanys property.
     * 
     * @return
     *     possible object is
     *     {@link SelectedCompanysType }
     *     
     */
    public SelectedCompanysType getSelectedCompanys() {
        return selectedCompanys;
    }

    /**
     * Sets the value of the selectedCompanys property.
     * 
     * @param value
     *     allowed object is
     *     {@link SelectedCompanysType }
     *     
     */
    public void setSelectedCompanys(SelectedCompanysType value) {
        this.selectedCompanys = value;
    }

    /**
     * Gets the value of the selectedIco property.
     * 
     * @return
     *     possible object is
     *     {@link SelectedIcoType }
     *     
     */
    public SelectedIcoType getSelectedIco() {
        return selectedIco;
    }

    /**
     * Sets the value of the selectedIco property.
     * 
     * @param value
     *     allowed object is
     *     {@link SelectedIcoType }
     *     
     */
    public void setSelectedIco(SelectedIcoType value) {
        this.selectedIco = value;
    }

    /**
     * Gets the value of the lastChanges property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getLastChanges() {
        return lastChanges;
    }

    /**
     * Sets the value of the lastChanges property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setLastChanges(XMLGregorianCalendar value) {
        this.lastChanges = value;
    }

}
