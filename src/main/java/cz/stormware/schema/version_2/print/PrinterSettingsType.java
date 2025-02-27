//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:43 PM CEST 
//


package cz.stormware.schema.version_2.print;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for printerSettingsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="printerSettingsType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="report" type="{http://www.stormware.cz/schema/version_2/print.xsd}reportType"/&gt;
 *         &lt;element name="printer" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="pdf" type="{http://www.stormware.cz/schema/version_2/print.xsd}PDFType" minOccurs="0"/&gt;
 *         &lt;element name="parameters" type="{http://www.stormware.cz/schema/version_2/print.xsd}parametersType" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "printerSettingsType", propOrder = {

})
public class PrinterSettingsType {

    @XmlElement(required = true)
    protected ReportType report;
    protected String printer;
    protected PDFType pdf;
    protected ParametersType parameters;

    /**
     * Gets the value of the report property.
     * 
     * @return
     *     possible object is
     *     {@link ReportType }
     *     
     */
    public ReportType getReport() {
        return report;
    }

    /**
     * Sets the value of the report property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReportType }
     *     
     */
    public void setReport(ReportType value) {
        this.report = value;
    }

    /**
     * Gets the value of the printer property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPrinter() {
        return printer;
    }

    /**
     * Sets the value of the printer property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPrinter(String value) {
        this.printer = value;
    }

    /**
     * Gets the value of the pdf property.
     * 
     * @return
     *     possible object is
     *     {@link PDFType }
     *     
     */
    public PDFType getPdf() {
        return pdf;
    }

    /**
     * Sets the value of the pdf property.
     * 
     * @param value
     *     allowed object is
     *     {@link PDFType }
     *     
     */
    public void setPdf(PDFType value) {
        this.pdf = value;
    }

    /**
     * Gets the value of the parameters property.
     * 
     * @return
     *     possible object is
     *     {@link ParametersType }
     *     
     */
    public ParametersType getParameters() {
        return parameters;
    }

    /**
     * Sets the value of the parameters property.
     * 
     * @param value
     *     allowed object is
     *     {@link ParametersType }
     *     
     */
    public void setParameters(ParametersType value) {
        this.parameters = value;
    }

}
