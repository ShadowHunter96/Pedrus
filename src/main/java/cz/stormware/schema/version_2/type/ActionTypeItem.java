//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.type;

import cz.stormware.schema.version_2.filter.RequestItemType;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for actionTypeItem complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="actionTypeItem"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;choice&gt;
 *         &lt;element name="add" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="update" type="{http://www.stormware.cz/schema/version_2/filter.xsd}requestItemType"/&gt;
 *         &lt;element name="delete" type="{http://www.stormware.cz/schema/version_2/filter.xsd}requestItemType"/&gt;
 *       &lt;/choice&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "actionTypeItem", propOrder = {
    "add",
    "update",
    "delete"
})
public class ActionTypeItem {

    protected String add;
    protected RequestItemType update;
    protected RequestItemType delete;

    /**
     * Gets the value of the add property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAdd() {
        return add;
    }

    /**
     * Sets the value of the add property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAdd(String value) {
        this.add = value;
    }

    /**
     * Gets the value of the update property.
     * 
     * @return
     *     possible object is
     *     {@link RequestItemType }
     *     
     */
    public RequestItemType getUpdate() {
        return update;
    }

    /**
     * Sets the value of the update property.
     * 
     * @param value
     *     allowed object is
     *     {@link RequestItemType }
     *     
     */
    public void setUpdate(RequestItemType value) {
        this.update = value;
    }

    /**
     * Gets the value of the delete property.
     * 
     * @return
     *     possible object is
     *     {@link RequestItemType }
     *     
     */
    public RequestItemType getDelete() {
        return delete;
    }

    /**
     * Sets the value of the delete property.
     * 
     * @param value
     *     allowed object is
     *     {@link RequestItemType }
     *     
     */
    public void setDelete(RequestItemType value) {
        this.delete = value;
    }

}
