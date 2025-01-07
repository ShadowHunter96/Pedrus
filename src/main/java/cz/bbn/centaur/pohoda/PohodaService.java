package cz.bbn.cerberus.pohoda;


import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.xml.XmlUtils;
import cz.bbn.cerberus.invoice.InvoicingService;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.translation.Transl;
import cz.stormware.schema.version_2.data.DataPackItemType;
import cz.stormware.schema.version_2.data.DataPackType;
import cz.stormware.schema.version_2.invoice.InvoiceHeaderType;
import cz.stormware.schema.version_2.invoice.InvoiceSummaryType;
import cz.stormware.schema.version_2.invoice.InvoiceType;
import cz.stormware.schema.version_2.invoice.InvoiceTypeType;
import cz.stormware.schema.version_2.invoice.ObjectFactory;
import cz.stormware.schema.version_2.print.AgendaPrintType;
import cz.stormware.schema.version_2.print.IsdocType;
import cz.stormware.schema.version_2.print.PDFType;
import cz.stormware.schema.version_2.print.ParametersType;
import cz.stormware.schema.version_2.print.PrinterSettingsType;
import cz.stormware.schema.version_2.print.ReportType;
import cz.stormware.schema.version_2.type.Boolean;
import cz.stormware.schema.version_2.type.LiquidationType;
import cz.stormware.schema.version_2.type.NumberType;
import cz.stormware.schema.version_2.type.TypeCalculateVATInclusivePriceType;
import cz.stormware.schema.version_2.type.TypeCurrencyHome;
import cz.stormware.schema.version_2.type.TypeRoundingDocument;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBElement;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.springframework.stereotype.Service;

import javax.xml.datatype.DatatypeConfigurationException;
import java.io.IOException;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

@Service
public class PohodaService {

    private final InvoicingService invoicingService;
    private final AppLogService appLogService;
    private final AppEnv appEnv;

    public PohodaService(InvoicingService invoicingService, AppLogService appLogService, AppEnv appEnv) {
        this.invoicingService = invoicingService;
        this.appLogService = appLogService;
        this.appEnv = appEnv;
    }

    public void createInvoice(InvoiceDto dto) throws JAXBException, IOException, DatatypeConfigurationException {
        cz.stormware.schema.version_2.data.ObjectFactory data = new cz.stormware.schema.version_2.data.ObjectFactory();
        StringWriter sw = new StringWriter();
        DataPackType dataPack = createDataPackType(data);

        JAXBElement<DataPackType> dataPackElement = data.createDataPack(dataPack);
        DataPackItemType dataPackItemType = createDataPackItemType(data);
        dataPackElement.getValue().getDataPackItem().add(dataPackItemType);

        ObjectFactory invoiceFactory = new ObjectFactory();
        InvoiceType invoice = createInvoiceType(invoiceFactory);

        InvoiceHeaderType invoiceHeader = createInvoiceHeaderType(dto);
        InvoiceSummaryType invoiceSummary = createInvoiceSummaryType(dto);

        invoice.setInvoiceHeader(invoiceHeader);
        invoice.setInvoiceSummary(invoiceSummary);

        AgendaPrintType agendaPrintType = createAgendaPrintType(dto);
        invoice.setPrint(agendaPrintType);

        dataPackItemType.setAny(invoiceFactory.createInvoice(invoice));

        Marshaller marshaller = JAXBContext.newInstance(
                DataPackType.class, InvoiceType.class).createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FRAGMENT, true);

        marshaller.marshal(dataPackElement, sw);

        String message = doPost(sw.toString());
        appLogService.log("Pohoda response message", message, String.valueOf(dto.getId()));
        if (!message.contains("state=\"ok\"") || message.contains("state=\"error\"")) {
            ErrorNotification.show(Transl.get("Error occurred, please contact administrator"), appEnv);
        } else {
            invoicingService.updateCreatedInPohoda(dto.getId());
            SuccessNotification.show(Transl.get("Imported successfully"), appEnv);
        }

    }

    private String doPost(String xmlString) throws IOException {
        HttpPost postRequest = new HttpPost(appEnv.getPohodaUrl());
        postRequest.addHeader("STW-Authorization", appEnv.getPohodaUserPassword());
        postRequest.addHeader("Content-type", "text/xml");
        postRequest.addHeader("Accept-Encoding", "Windows-1250");

        HttpEntity entity = new ByteArrayEntity(xmlString.getBytes(StandardCharsets.UTF_8));
        postRequest.setEntity(entity);
        CloseableHttpClient httpclient = HttpClients.createDefault();
        try {
            HttpResponse response = httpclient.execute(postRequest);
            return EntityUtils.toString(response.getEntity());
        } finally {
            httpclient.close();
        }
    }

    private DataPackType createDataPackType(cz.stormware.schema.version_2.data.ObjectFactory data) {
        DataPackType dataPack = data.createDataPackType();
        dataPack.setIco(appEnv.getPohodaIco());
        dataPack.setVersion("2.0");
        dataPack.setApplication("cerberus");
        dataPack.setNote("create invoice");
        dataPack.setId("Usr01");
        return dataPack;
    }

    private DataPackItemType createDataPackItemType(cz.stormware.schema.version_2.data.ObjectFactory data) {
        DataPackItemType dataPackItem = data.createDataPackItemType();
        dataPackItem.setVersion("2.0");
        dataPackItem.setId("Usr01");
        return dataPackItem;
    }

    private InvoiceType createInvoiceType(ObjectFactory invoiceFactory) {
        InvoiceType invoice = invoiceFactory.createInvoiceType();
        invoice.setVersion("2.0");
        return invoice;
    }

    private InvoiceHeaderType createInvoiceHeaderType(InvoiceDto dto) throws DatatypeConfigurationException {
        InvoiceHeaderType invoiceHeader = new InvoiceHeaderType();
        invoiceHeader.setInvoiceType(InvoiceTypeType.ISSUED_INVOICE);
        invoiceHeader.setText(dto.getDescription());
        NumberType numberType = new NumberType();
        numberType.setNumberRequested(new NumberType.NumberRequested());
        numberType.getNumberRequested().setValue("CERB".concat(String.valueOf(dto.getId())));
        invoiceHeader.setNumber(numberType);
        invoiceHeader.setDateDue(XmlUtils.toXmlGregorianCalendar(dto.getIssueDate()));
        invoiceHeader.setDate(XmlUtils.toXmlGregorianCalendar(dto.getPaymentDate()));
        LiquidationType liquidationType = new LiquidationType();
        liquidationType.setAmountHome(Optional.ofNullable(dto.getPriceNoVat()).orElse(0D));
        invoiceHeader.setLiquidation(liquidationType);
        return invoiceHeader;
    }

    private InvoiceSummaryType createInvoiceSummaryType(InvoiceDto dto) {
        InvoiceSummaryType invoiceSummary = new InvoiceSummaryType();
        invoiceSummary.setRoundingDocument(TypeRoundingDocument.NONE);
        invoiceSummary.setCalculateVAT(java.lang.Boolean.TRUE);
        invoiceSummary.setTypeCalculateVATInclusivePrice(TypeCalculateVATInclusivePriceType.VAT_NEW_METHOD);
        TypeCurrencyHome typeCurrencyHome = new TypeCurrencyHome();
        typeCurrencyHome.setPriceHighSum(Optional.ofNullable(dto.getPriceNoVat()).orElse(0D));
        invoiceSummary.setHomeCurrency(typeCurrencyHome);
        return invoiceSummary;
    }

    private AgendaPrintType createAgendaPrintType(InvoiceDto dto) {
        cz.stormware.schema.version_2.print.ObjectFactory printFactory =
                new cz.stormware.schema.version_2.print.ObjectFactory();
        AgendaPrintType agendaPrint = new AgendaPrintType();
        PrinterSettingsType printerSettings = printFactory.createPrinterSettingsType();
        PDFType pdf = printFactory.createPDFType();
        pdf.setFileName("CERB".concat(String.valueOf(dto.getId())));
        IsdocType isdoc = printFactory.createIsdocType();
        isdoc.setIncludeToPdf(Boolean.TRUE);
        isdoc.setGraphicNote("topLeft");
        pdf.setIsdoc(isdoc);
        printerSettings.setPdf(pdf);

        ReportType report = printFactory.createReportType();
        report.setId(BigInteger.valueOf(190));
        printerSettings.setReport(report);

        ParametersType parameters = printFactory.createParametersType();
        parameters.setCopy(BigDecimal.ONE);
        printerSettings.setParameters(parameters);

        agendaPrint.getPrinterSettings().add(printerSettings);
        return agendaPrint;
    }

}
