//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.type;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for manualLinkElementType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="manualLinkElementType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="sourceAgenda" type="{http://www.stormware.cz/schema/version_2/type.xsd}agendaType"/&gt;
 *         &lt;element name="sourceDocument" type="{http://www.stormware.cz/schema/version_2/type.xsd}sourceDocumentType"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "manualLinkElementType", propOrder = {

})
public class ManualLinkElementType {

    @XmlElement(required = true)
    @XmlSchemaType(name = "string")
    protected AgendaType sourceAgenda;
    @XmlElement(required = true)
    protected SourceDocumentType sourceDocument;

    /**
     * Gets the value of the sourceAgenda property.
     * 
     * @return
     *     possible object is
     *     {@link AgendaType }
     *     
     */
    public AgendaType getSourceAgenda() {
        return sourceAgenda;
    }

    /**
     * Sets the value of the sourceAgenda property.
     * 
     * @param value
     *     allowed object is
     *     {@link AgendaType }
     *     
     */
    public void setSourceAgenda(AgendaType value) {
        this.sourceAgenda = value;
    }

    /**
     * Gets the value of the sourceDocument property.
     * 
     * @return
     *     possible object is
     *     {@link SourceDocumentType }
     *     
     */
    public SourceDocumentType getSourceDocument() {
        return sourceDocument;
    }

    /**
     * Sets the value of the sourceDocument property.
     * 
     * @param value
     *     allowed object is
     *     {@link SourceDocumentType }
     *     
     */
    public void setSourceDocument(SourceDocumentType value) {
        this.sourceDocument = value;
    }

}
