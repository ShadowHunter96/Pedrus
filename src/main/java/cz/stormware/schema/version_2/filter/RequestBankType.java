//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.filter;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for requestBankType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="requestBankType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;group ref="{http://www.stormware.cz/schema/version_2/filter.xsd}groupFilter_1"/&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "requestBankType", propOrder = {
    "filter",
    "userFilterName"
})
public class RequestBankType {

    protected FilterDocsType filter;
    protected String userFilterName;

    /**
     * Gets the value of the filter property.
     * 
     * @return
     *     possible object is
     *     {@link FilterDocsType }
     *     
     */
    public FilterDocsType getFilter() {
        return filter;
    }

    /**
     * Sets the value of the filter property.
     * 
     * @param value
     *     allowed object is
     *     {@link FilterDocsType }
     *     
     */
    public void setFilter(FilterDocsType value) {
        this.filter = value;
    }

    /**
     * Gets the value of the userFilterName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getUserFilterName() {
        return userFilterName;
    }

    /**
     * Sets the value of the userFilterName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setUserFilterName(String value) {
        this.userFilterName = value;
    }

}
