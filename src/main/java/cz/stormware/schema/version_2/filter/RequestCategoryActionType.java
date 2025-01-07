//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.filter;

import cz.stormware.schema.version_2.type.Boolean;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for requestCategoryActionType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="requestCategoryActionType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;group ref="{http://www.stormware.cz/schema/version_2/filter.xsd}groupFilter_2"/&gt;
 *       &lt;attGroup ref="{http://www.stormware.cz/schema/version_2/filter.xsd}groupAttributeAction"/&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "requestCategoryActionType", propOrder = {
    "filter",
    "userFilterName"
})
public class RequestCategoryActionType {

    protected FilterIDType filter;
    protected String userFilterName;
    @XmlAttribute(name = "update")
    protected Boolean update;
    @XmlAttribute(name = "add")
    protected Boolean add;

    /**
     * Gets the value of the filter property.
     * 
     * @return
     *     possible object is
     *     {@link FilterIDType }
     *     
     */
    public FilterIDType getFilter() {
        return filter;
    }

    /**
     * Sets the value of the filter property.
     * 
     * @param value
     *     allowed object is
     *     {@link FilterIDType }
     *     
     */
    public void setFilter(FilterIDType value) {
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

    /**
     * Gets the value of the update property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean getUpdate() {
        if (update == null) {
            return Boolean.FALSE;
        } else {
            return update;
        }
    }

    /**
     * Sets the value of the update property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setUpdate(Boolean value) {
        this.update = value;
    }

    /**
     * Gets the value of the add property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean getAdd() {
        if (add == null) {
            return Boolean.FALSE;
        } else {
            return add;
        }
    }

    /**
     * Sets the value of the add property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setAdd(Boolean value) {
        this.add = value;
    }

}
