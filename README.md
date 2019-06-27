# BluAge Serverless COBOL for AWS

## A short story of serverless COBOL for AWS

During the last quarter of 2018, we introduced the [serverless COBOL for AWS](https://www.bluage.com/products/serverless-cobol) solution, making use of [AWS Lambda Layers](https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html) (officially presented during [AWS Re:invent 2018](https://www.youtube.com/watch?v=femopq3JWJg&feature=youtu.be&t=4621)) and our COBOL to java compiler.

For the first time, COBOL developers were able to compile and deploy their code to [AWS Lambda](https://aws.amazon.com/lambda/?nc2=h_m1) functions, using their favourite language, an universal IDE ([Visual Studio Code](https://code.visualstudio.com/)) and a remote compiler service, hosted on AWS.

During the first quarter of 2019, we wrote dedicated extensions to permit the COBOL code to interact with popular AWS services ([SQS](https://aws.amazon.com/sqs/?nc2=h_m1), [S3](https://aws.amazon.com/s3/?nc2=h_m1), [Kinesis](https://aws.amazon.com/kinesis/?nc2=h_m1), [API Gateway](https://aws.amazon.com/api-gateway/?nc2=h_m1), [DynamoDB](https://aws.amazon.com/dynamodb/?nc2=h_m1)) and handle respective triggers for Lambda functions.

At [AWS Summit Tokyo 2019](https://aws.amazon.com/jp/summits/tokyo-2019/), in partnership with [Accenture Japan](https://www.accenture.com/jp-ja), we demonstrated how we managed to migrate a CICS Online COBOL application into a serverless web application, relying on Lambda functions for the backend (see the [video](https://youtu.be/jhB39NlgGl4?t=2806)). 

## Try for yourself!

You can register for a free serverless COBOL trial, using this [registration page](https://www.bluage.com/contact/contact-serverless-cobol).

Our helpdesk team will provide the required resources to get you started compiling your COBOL code and deploy it to an AWS lambda function, on a region nearby your location.

Meanwhile, you can browse the documentation:
- [COBOL compiler user guide](./VSCodeCompilerForCOBOL-UserGuide.md)
- [COBOL extensions for AWS - programming guide](./ServerlessCobolAWSExtensions.md)
- sample COBOL programs 




